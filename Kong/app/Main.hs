module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    putStrLn "Kong"
    exitSuccess
