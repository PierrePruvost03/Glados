module Main (main, getUserInput) where

-- import Lib

import GHC.IO.StdHandles
import System.Exit (exitSuccess)
import Interpreter.BaseEnv (defaultEnv)
import GetInput (getUserInput)

main :: IO ()
main = getUserInput "" defaultEnv stdin >> putStrLn "exit" >> exitSuccess
