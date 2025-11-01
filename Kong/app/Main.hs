module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import ExecuteVM (executeVM)
import Loader (loadAndValidateFiles)
import ErrorPrinter (printIncludeError)
import Runtime (compileAndExecute)

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [] = printUsage >> exitFailure
handleArgs ("--exec" : file : []) = executeVM file >>= exitWith . toExitCode
handleArgs files = loadAndValidateFiles files >>= \result ->
    handleLoad result printIncludeError compileAndExecute

printUsage :: IO ()
printUsage = hPutStrLn stderr $ unlines
    [ "Usage: glados [OPTIONS] <source-file> [additional-files...]"
    , ""
    , "Options:"
    , "  --exec <file>    Execute a pre-compiled bytecode file"
    , ""
    , "Examples:"
    , "  glados main.kong lib.kong         # Compile multiple Kong files"
    , "  glados --exec program.bytecode    # Execute pre-compiled bytecode"
    ]

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode n = ExitFailure n

-- Load files and handle the result
handleLoad :: Either a [(String, b)] -> (a -> IO ()) -> ([(String, b)] -> IO ()) -> IO ()
handleLoad (Left err) onError _ = onError err
handleLoad (Right fileAsts) _ onSuccess = onSuccess fileAsts
