module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode (..) )
import System.IO (hPutStrLn, stderr)
import Parser (runParser)
import DataStruct.VM (baseState, VMState (VMState, stack))
import AstParsing.BaseParsing (parseAst)
import Compiler.Program (compileProgram)
import VM.Execution (exec)
import DataStruct.Ast (Ast)
import DataStruct.Bytecode.Value (Instr(..), Value (VNumber))
import DataStruct.Bytecode.Number (Number(VInt))
import AstParsing.ErrorMessage

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [file] = readFile file >>= handleParse
handleArgs _ = hPutStrLn stderr "Usage: glados <source-file>" >> exitFailure

handleParse :: String -> IO ()
handleParse content =
    either handleCompile printParseError $ printParsingResult content parseAst

handleCompile :: [Ast] -> IO ()
handleCompile asts =
    either printCompileError handleExec $ compileProgram [("main", asts)]

printParseError :: String -> IO ()
printParseError str =
    hPutStrLn stderr str >> exitFailure

printCompileError :: Show e => e -> IO ()
printCompileError errs = hPutStrLn stderr ("[Compilation error] " ++ show errs) >> exitFailure

handleExec :: [Instr] -> IO ()
handleExec instrs = do
    putStrLn $ "[Bytecode] " ++ show instrs --debug
    exec (baseState instrs) >>= printResult

printResult :: VMState -> IO ()
printResult result@(VMState {stack = (VNumber (VInt r)) : _}) =
    putStrLn ("[Execution finished] Final VM state: " ++ show result) >> returnValue r
printResult result = putStrLn ("[Execution finished] Final VM state: " ++ show result ++
    "\n invalid return type") >> returnValue 1


returnValue :: Int -> IO ()
returnValue 0 = exitSuccess
returnValue n = exitWith (ExitFailure n)