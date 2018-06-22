module Main where

import System.Environment
import System.Process
import GHC.IO.Handle
import System.Exit
import qualified Data.ByteString as BS
import Data.Monoid

getFilePath :: IO (Maybe FilePath)
getFilePath = do 
  args <- getArgs
  case args of 
    [] -> return Nothing
    (x:_) -> return . Just $ x

listLocalBranchesProcessDescription :: Maybe FilePath -> CreateProcess
listLocalBranchesProcessDescription workingDir = 
    (shell "git branch"){ cwd = workingDir
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
          last <- BS.hGetContents h
          return (ec, acc' <> last)

main :: IO ()
main = do
  filePath <- getFilePath
  (_, Just hStdOut, _, handle) <- createProcess (listLocalBranchesProcessDescription filePath)
  (exitCode, output) <- gatherOutput handle hStdOut
  putStrLn $ show output
  return ()
 
