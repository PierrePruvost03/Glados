{-# LANGUAGE OverloadedStrings #-}
module Loader
    ( loadAndValidateFiles
    , parseFile
    , normalizeFilePath
    ) where

import DataStruct.Ast (Ast)
import AstParsing.BaseParsing (parseAst)
import AstParsing.ErrorMessage (printParsingResult)
import Compiler.Include (sortByDependencies, applySelectiveImports, validateIncludes, validateNoDuplicateSymbols, IncludeError(..))
import System.FilePath (dropExtension, takeDirectory, makeRelative)
import qualified Data.Set as S

-- Load, parse, validate and sort Kong source files
loadAndValidateFiles :: [FilePath] -> IO (Either IncludeError [(String, [Ast])])
loadAndValidateFiles filePaths =
  mapM (parseFile baseDir) filePaths >>= processFiles
  where
    baseDir = case filePaths of
      [] -> "."
      (first:_) -> takeDirectory first

    processFiles parsedFiles = case sequence parsedFiles of
      Left err -> return $ Left err
      Right fileAsts -> validateAndSort fileAsts

    validateAndSort fileAsts = case validateIncludes fileAsts providedFiles of
      Left err -> return $ Left err
      Right _ -> case validateNoDuplicateSymbols fileAsts of
        Left err -> return $ Left err
        Right _ -> sortAndFilter fileAsts

    sortAndFilter fileAsts = case sortByDependencies fileAsts of
      Left err -> return $ Left err
      Right sorted -> return $ applySelectiveImports sorted

    providedFiles = S.fromList $ map (normalizeFilePath baseDir) filePaths

-- Parse a Kong source file into an AST
parseFile :: FilePath -> FilePath -> IO (Either IncludeError (String, [Ast]))
parseFile baseDir filePath =
  readFile filePath >>= parseContent
  where
    fileName = normalizeFilePath baseDir filePath

    parseContent content = case printParsingResult content parseAst of
      Right str ->
        return $ Left $ ParseError filePath str
      Left asts -> return $ Right (fileName, asts)

-- Normalize a file path by removing extension and making it relative to base directory (for include process)
normalizeFilePath :: FilePath -> FilePath -> String
normalizeFilePath baseDir path = dropExtension (makeRelative baseDir path)
