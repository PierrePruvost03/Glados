module ErrorPrinter
    ( printIncludeError
    , printCompileError
    ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Compiler.Include (IncludeError(..))
import Compiler.Type.Error (ProgramError(..), prettyError)

-- Print an include error to stderr and exit
printIncludeError :: IncludeError -> IO ()
printIncludeError (FileNotFound path) =
    hPutStrLn stderr ("[Include error] File not found: " ++ path) >> exitFailure
printIncludeError (CircularDependency file chain) =
    hPutStrLn stderr ("[Include error] Circular dependency detected in " ++ file ++
                     ": " ++ show chain) >> exitFailure
printIncludeError (ParseError _ msg) =
    hPutStrLn stderr ("[Parsing error] " ++ msg) >> exitFailure
printIncludeError (MissingInclude file missing) =
    hPutStrLn stderr ("[Include error] File '" ++ file ++ "' includes '" ++ missing ++
                     "' but it was not provided in compilation arguments") >> exitFailure
printIncludeError (MissingSymbol requester included symbol) =
    hPutStrLn stderr ("[Include error] File '" ++ requester ++ "' requests symbol '" ++ symbol ++
                     "' from '" ++ included ++ "' but it does not exist") >> exitFailure
printIncludeError (DuplicateSymbol symbol files) =
    hPutStrLn stderr ("[Include error] Symbol '" ++ symbol ++ "' is defined in multiple files: " ++
                     show files) >> exitFailure

-- Print compilation errors to stderr and exit
printCompileError :: [ProgramError] -> IO ()
printCompileError errs = do
    hPutStrLn stderr "[Compilation error]"
    mapM_ (\(ProgramError file _ err) -> 
        hPutStrLn stderr $ "  In file '" ++ file ++ "': " ++ prettyError err) errs
    exitFailure
