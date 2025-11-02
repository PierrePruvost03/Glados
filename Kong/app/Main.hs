module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import ExecuteVM (executeVM)
import Loader (loadAndValidateFiles)
import ErrorPrinter (printIncludeError, printCompileError)
import Runtime (writeToBinary)
import Compiler.BytecodeGen.Program.Program (compileProgram)
import Compiler.Include (IncludeError(..))
import DataStruct.Ast (Ast)
import System.Exit


main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [] = printUsage >> exitFailure
handleArgs ("--exec" : file : xs) = executeVM file xs >>= exitWith . toExitCode
handleArgs files = loadAndValidateFiles execfiles >>= \result ->
    handleLoad result binary
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

handleLoad :: Either IncludeError [(String, [Ast])] -> String -> IO ()
handleLoad (Left err) _ = printIncludeError err
handleLoad (Right fileAsts) binary = compileAndWrite binary fileAsts
    where

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode n = ExitFailure n
