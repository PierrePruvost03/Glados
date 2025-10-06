module Main (main, getUserInput) where

-- import Lib

import GHC.IO.StdHandles
import System.Exit (exitSuccess)
import Interpreter.BaseEnv (defaultEnv)
import GetInput (getUserInput, loadFiles)
import System.Environment (getArgs)

main :: IO ()
main = getArgs
    >>= \arguments -> loadFiles defaultEnv arguments
    >>= \newEnv -> getUserInput "" defaultEnv stdin >> putStrLn "exit" >> exitSuccess
