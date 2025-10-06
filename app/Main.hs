module Main (main, getUserInput) where

-- import Lib

import GHC.IO.StdHandles
import GetInput (getUserInput)
import Interpreter.Env.BaseEnv (defaultEnv)
import System.Exit (exitSuccess)

main :: IO ()
main = getUserInput "" defaultEnv stdin >> putStrLn "exit" >> exitSuccess
