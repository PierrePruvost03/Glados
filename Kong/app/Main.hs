module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode (..) )
import System.IO (hPutStrLn, stderr)
import DataStruct.VM (baseState, VMState (VMState, stack))
import Compiler.Program (compileProgram)
import Compiler.Include (validateIncludes, sortByDependencies, applySelectiveImports, IncludeError(..))
import VM.Execution (exec)
import DataStruct.Ast (Ast)
import DataStruct.Bytecode.Value (Instr(..), Value (VNumber))
import DataStruct.Bytecode.Number (Number(VInt))
import Parser (runParser)
import AstParsing.BaseParsing (parseAst)
import System.FilePath (dropExtension, takeFileName)
import qualified Data.Set as S

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [] = hPutStrLn stderr "Usage: glados <source-file> [additional-files...]" >> exitFailure
handleArgs files = loadAndValidateFiles files >>= handleLoad

loadAndValidateFiles :: [FilePath] -> IO (Either IncludeError [(String, [Ast])])
loadAndValidateFiles filePaths =
  mapM parseFile filePaths >>= processFiles
  where
    processFiles parsedFiles = case sequence parsedFiles of
      Left err -> return $ Left err
      Right fileAsts -> validateAndSort fileAsts

    validateAndSort fileAsts = case validateIncludes fileAsts providedFiles of
      Left err -> return $ Left err
      Right _ -> sortAndFilter fileAsts

    sortAndFilter fileAsts = case sortByDependencies fileAsts of
      Left err -> return $ Left err
      Right sorted -> return $ Right $ applySelectiveImports sorted

    providedFiles = S.fromList $ map (dropExtension . takeFileName) filePaths

handleParse :: String -> IO ()
handleParse content =
    either printParseError handleCompile $ runParser parseAst (content, (1, 1))

parseFile :: FilePath -> IO (Either IncludeError (String, [Ast]))
parseFile filePath =
  readFile filePath >>= parseContent
  where
    fileName = dropExtension $ takeFileName filePath

    parseContent content = case runParser parseAst (content, (1, 1)) of
      Left (_, scope, detail, (line, col)) ->
        return $ Left $ ParseError filePath
          (scope ++ ": " ++ detail ++ " at line " ++ show line ++ ", col " ++ show col)
      Right (asts, _) -> return $ Right (fileName, asts)

handleLoad :: Either IncludeError [(String, [Ast])] -> IO ()
handleLoad (Left err) = printIncludeError err
handleLoad (Right fileAsts) = handleCompile fileAsts

handleCompile :: [(String, [Ast])] -> IO ()
handleCompile fileAsts =
    either printCompileError handleExec $ compileProgram fileAsts

printParseError :: String -> IO ()
printParseError str =
    hPutStrLn stderr str >> exitFailure

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

handleCompile :: [(String, [Ast])] -> IO ()
handleCompile fileAsts =
    either printCompileError handleExec $ compileProgram fileAsts

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