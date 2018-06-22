{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process
import GHC.IO.Handle
import System.Exit
import qualified Data.ByteString.Char8 as BS
import Data.Semigroup ((<>))
import Control.Monad.Reader
import Data.List
import Options.Applicative

data Config = Config { configFilePath :: FilePath
                     , configShouldPrune :: Bool
                     , configDebugMode :: Bool
                     }

config :: Parser Config
config = Config
     <$> strOption
         ( long "folder"
        <> short 'f'
        <> metavar "FOLDER"
        <> help "Folder path to perform operations" 
        <> showDefault
        <> value ".")
     <*> switch
         ( long "prune"
        <> short 'p'
        <> help "Should prune remote" )
     <*> switch
        ( long "debug"
       <> short 'd'
       <> help "Debug mode" )

type App = ReaderT Config IO

constructProcess :: FilePath -> String -> CreateProcess
constructProcess workingDir cmd = 
    (shell cmd){ cwd = Just workingDir
                        , std_out = CreatePipe
                        , std_err = CreatePipe 
                        } 

gatherOutput :: ProcessHandle -> Handle -> IO (ExitCode, BS.ByteString)                        
gatherOutput ph h = work mempty
  where
    work acc = do
      -- Read any outstanding input
      bs <- BS.hGetNonBlocking h (64 * 1024)
      let acc' = acc <> bs
      -- Check on the process
      s <- getProcessExitCode ph
      case s of
        Nothing -> work acc'
        Just ec -> do
          -- Get any last bit written between the read and the status
          -- check
          l <- BS.hGetContents h
          return (ec, acc' <> l)

getProcessOutput :: String -> App BS.ByteString
getProcessOutput cmd = do
  Config filePath _ debugMode <- ask  
  when debugMode $ liftIO $ putStrLn $ "Executing command: " <> cmd
  (_, Just hStdOut, _, handle) <- lift $ createProcess (constructProcess filePath cmd)
  (_, output) <- lift $ gatherOutput handle hStdOut
  return output

getLocalBranches :: App [BS.ByteString]
getLocalBranches =  
  getProcessOutput "git branch" >>=
    return . filter ((/=) "master") . concat . fmap (filter ((/=) "*") . BS.words) . BS.lines

getRemoteBranchName :: BS.ByteString -> BS.ByteString -- origin/feature/test -> feature/test
getRemoteBranchName = BS.intercalate "/" . drop 1 . BS.split '/'

getRemoteBranches :: App [BS.ByteString]
getRemoteBranches =
  getProcessOutput "git branch -r" >>= 
    return . filter ((/=) "master") . fmap getRemoteBranchName . BS.lines

getRemote :: App [BS.ByteString]
getRemote = getProcessOutput "git remote" >>= (return . BS.lines)

pruneRemoteBranches :: BS.ByteString -> App ()
pruneRemoteBranches remote = 
  getProcessOutput ("git remote prune " <> BS.unpack remote) >> return ()

runApp :: App ()
runApp = do
  remotes <- getRemote
  Config _ shouldPrune debugMode <- ask
  case remotes of
    (remote:_) -> do
      localBranches <- getLocalBranches
      remoteBranches <- getRemoteBranches
      when debugMode $ do
        liftIO $ putStrLn $ "Local " <> show localBranches
        liftIO $ putStrLn $ "Remote " <> show remoteBranches
        liftIO $ putStrLn $ "Diff " <> show (localBranches \\ remoteBranches)
      when shouldPrune (pruneRemoteBranches remote)
    [] ->
      liftIO $ putStrLn "Git Repository does not have a remote set up"
  
main :: IO ()
main = do
  execParser opts >>= runReaderT runApp
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Prune local branches"
     <> header "git-pruner - Prune local git branches" )
 
