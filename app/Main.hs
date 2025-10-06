{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
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
    >>= \newEnv -> getUserInput "" newEnv stdin >> putStrLn "exit" >> exitSuccess
