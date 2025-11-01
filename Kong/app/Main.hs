module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import ExecuteVM (executeVM)
import Loader (loadAndValidateFiles)
import ErrorPrinter (printIncludeError, printCompileError)
import Runtime (compileAndExecute, writeToBinary)
import Compiler.BytecodeGen.Program.Program (compileProgram)
import DataStruct.Ast (Ast)

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [] = printUsage >> exitFailure
handleArgs ("--exec" : file : []) = executeVM file >>= exitWith . toExitCode
handleArgs files = loadAndValidateFiles execfiles >>= \result ->
    handleLoad result printIncludeError (compileAndWrite binary)
    where
        (binary, execfiles) = getComputeData files ("strong.out", [])

getComputeData :: [String] -> (String, [String]) -> (String, [String])
getComputeData [] r = r
getComputeData ("-o" : name : xs) (_, r) = getComputeData xs (name, r)
getComputeData (x:xs) (n, r) = getComputeData xs (n, x:r)

printUsage :: IO ()
printUsage = hPutStrLn stderr $ unlines
    [ "Usage: glados [OPTIONS] <source-file> [additional-files...]"
    , ""
    , "Options:"
    , "  --exec <file>              Execute a pre-compiled bytecode file"
    , "  -o <output binary name>    set the output binary name (default strong.out)"
    , ""
    , "Examples:"
    , "  glados main.kong lib.kong         # Compile multiple Kong files"
    , "  glados --exec program.bytecode    # Execute pre-compiled bytecode"
    ]


compileAndWrite :: String -> [(String, [Ast])] -> IO ()
compileAndWrite bName toComp = either printCompileError (writeToBinary bName) $ compileProgram toComp

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode n = ExitFailure n

-- Load files and handle the result
handleLoad :: Either a [(String, [Ast])] -> (a -> IO ()) -> ([(String, [Ast])] -> IO ()) -> IO ()
handleLoad (Left err) onError _ = onError err
handleLoad (Right fileAsts) _ onSuccess = onSuccess fileAsts
