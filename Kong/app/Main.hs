module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Parser (runParser)
import AstParsing.BaseParsing (parseAst)
import Compiler.Program (compileProgram)
import DataStruct.VM (initVMState)
import VM.Execution (exec)
import DataStruct.Ast (Ast)
import DataStruct.Bytecode.Value (Instr)

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [file] = readFile file >>= handleParse
handleArgs _ = hPutStrLn stderr "Usage: glados-kong <source-file>" >> exitFailure

handleParse :: String -> IO ()
handleParse content =
    either printParseError handleCompile $ runParser parseAst (content, (1, 1))

handleCompile :: ([Ast], b) -> IO ()
handleCompile (asts, _) =
    either printCompileError handleExec $ compileProgram [("main", asts)]

printParseError :: (Bool, String, String, (Int, Int)) -> IO ()
printParseError (_, scope, detail, (line, col)) =
    hPutStrLn stderr ("[Parsing error] " ++ scope ++ ": " ++ detail ++ " at line " ++ show line ++ ", col " ++ show col) >> exitFailure

printCompileError :: Show e => e -> IO ()
printCompileError errs = hPutStrLn stderr ("[Compilation error] " ++ show errs) >> exitFailure

handleExec :: [Instr] -> IO ()
handleExec instrs = exec (initVMState instrs) >>= printResult

printResult :: Show a => a -> IO ()
printResult result = putStrLn ("[Execution finished] Final VM state: " ++ show result) >> exitSuccess
