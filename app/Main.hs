module Main where

import Control.Exception (Exception, throwIO)
import Lib (unvigenere, vigenere)
import System.Environment (getArgs)
import System.IO (hGetLine, hPutStr, hWaitForInput, stdin, stdout)

data Timeout = Timeout String deriving (Eq, Show)

instance Exception Timeout

key :: [Char]
key = "asdasd"

withTimeout :: IO () -> IO ()
withTimeout action = do
  didTimeout <- hWaitForInput stdin 1000
  case didTimeout of
    True -> action
    False -> throwIO $ Timeout "Application timedout"

decryptIO :: IO ()
decryptIO = do
  input <- hGetLine stdin
  let coded = vigenere input key
  hPutStr stdout coded

encryptIO :: IO ()
encryptIO = do
  input <- hGetLine stdin
  let coded = unvigenere input key
  hPutStr stdout coded

main :: IO ()
main = do
  args <- getArgs
  let t =
        case "-t" `elem` args of
          True -> withTimeout
          False -> id
  let d = "-d" `elem` args
  let e = "-e" `elem` args
  case (d, e) of
    (True, _) -> t decryptIO
    (_, True) -> t encryptIO
    _ -> putStrLn $ "Invalid args. Got " ++ show args
