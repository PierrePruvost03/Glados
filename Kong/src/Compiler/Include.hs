{-# LANGUAGE LambdaCase #-}

module Compiler.Include
  ( resolveIncludes,
    loadFileWithIncludes,
    validateIncludes,
    validateNoDuplicateSymbols,
    sortByDependencies,
    applySelectiveImports,
    resolveIncludesRec,
    IncludeError(..),
  ) where

import DataStruct.Ast
import Compiler.Unwrap (Unwrappable(..))
import Parser (runParser)
import AstParsing.BaseParsing (parseAst)
import Compiler.Type.Normalization (typeToString)
import System.FilePath (takeDirectory, (</>), takeFileName, dropExtension, addExtension)
import System.Directory (doesFileExist)
import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map as M

data IncludeError
  = FileNotFound String
  | CircularDependency String [String]
  | ParseError String String
  | MissingInclude String String  -- file who ask to include, missing file
  | MissingSymbol String String String  -- file who ask to include, included file, missing symbol
  | DuplicateSymbol String [String]  -- symbol name, list of files defining it
  deriving (Show, Eq)

resolveIncludes :: FilePath -> String -> IO (Either IncludeError [(String, [Ast])])
resolveIncludes filePath content =
  fmap (fmap reverse) $ case runParser parseAst (content, (1, 1)) of
    Left (_, scope, detail, (line, col)) ->
      return $ Left $ ParseError filePath 
        (scope ++ ": " ++ detail ++ " at line " ++ show line ++ ", col " ++ show col)
    Right (asts, _) ->
      resolveIncludesRec baseDir fileName asts S.empty M.empty []
  where
    baseDir = takeDirectory filePath
    fileName = dropExtension $ takeFileName filePath

resolveIncludesRec :: FilePath -> String -> [Ast] -> S.Set String -> M.Map String [Ast]
  -> [(String, [Ast])] -> IO (Either IncludeError [(String, [Ast])])
resolveIncludesRec baseDir currentFile asts processing cache acc
  | S.member currentFile processing = 
      return $ Left $ CircularDependency currentFile (S.toList processing)
  | M.member currentFile cache = 
      return $ Right acc
  | otherwise = 
      foldM (processInclude baseDir newProcessing cache) (Right acc) includes
        >>= either 
              (return . Left)
              (\newAcc -> return $ Right $ (currentFile, filteredAsts) : newAcc)
  where
    newProcessing = S.insert currentFile processing
    includes = extractIncludes asts
    filteredAsts = filter (not . isInclude) asts

processInclude :: FilePath -> S.Set String -> M.Map String [Ast] -> Either IncludeError [(String, [Ast])]
  -> String -> IO (Either IncludeError [(String, [Ast])])
processInclude _ _ _ (Left err) _ = return $ Left err
processInclude baseDir processing cache (Right acc) includeFile =
  doesFileExist includePath >>= \case
    False -> return $ Left $ FileNotFound includePath
    True -> readFile includePath >>= parseAndResolve
  where
    includePath = baseDir </> addExtension includeFile "kong"
    parseAndResolve content = case runParser parseAst (content, (1, 1)) of
      Left (_, scope, detail, (line, col)) ->
        return $ Left $ ParseError includePath 
          (scope ++ ": " ++ detail ++ " at line " ++ show line ++ ", col " ++ show col)
      Right (includedAsts, _) -> 
        resolveIncludesRec baseDir includeFile includedAsts processing cache acc

extractIncludes :: [Ast] -> [String]
extractIncludes = foldr extractInclude []
  where
    extractInclude :: Ast -> [String] -> [String]
    extractInclude ast = case unwrap ast of
      AInclude from _ -> (from :)
      _ -> id

isInclude :: Ast -> Bool
isInclude ast = case unwrap ast of
  AInclude _ _ -> True
  _ -> False

loadFileWithIncludes :: FilePath -> IO (Either IncludeError [(String, [Ast])])
loadFileWithIncludes filePath = readFile filePath >>= resolveIncludes filePath

validateIncludes :: [(String, [Ast])] -> S.Set String -> Either IncludeError ()
validateIncludes fileAsts providedFiles = 
  case findMissingIncludes fileAsts providedFiles of
    [] -> Right ()
    (file, missing):_ -> Left $ MissingInclude file missing

findMissingIncludes :: [(String, [Ast])] -> S.Set String -> [(String, String)]
findMissingIncludes fileAsts providedFiles =
  [(fileName, includeName) 
  | (fileName, asts) <- fileAsts
  , includeName <- extractIncludes asts
  , not (S.member includeName providedFiles)]

sortByDependencies :: [(String, [Ast])] -> Either IncludeError [(String, [Ast])]
sortByDependencies fileAsts =
  case topologicalSort fileMap dependencies of
    Left cyc -> Left $ CircularDependency (head cyc) cyc
    Right sorted -> Right $ fmap (\name -> (name, fileMap M.! name)) sorted
  where
    fileMap = M.fromList fileAsts
    dependencies = [(name, extractIncludes asts) | (name, asts) <- fileAsts]

topologicalSort :: M.Map String [Ast] -> [(String, [String])] -> Either [String] [String]
topologicalSort fileMap deps = fmap reverse $ topSort [] S.empty S.empty (M.keys fileMap)
  where
    depMap = M.fromList deps
    topSort :: [String] -> S.Set String -> S.Set String -> [String] -> Either [String] [String]
    topSort result _ _ [] = Right result
    topSort result visited visiting (node:rest)
      | S.member node visited = topSort result visited visiting rest
      | S.member node visiting = Left [node]
      | otherwise = 
          either 
            (Left . (node :))
            (\depResults -> topSort (node : (depResults ++ result)) newVisited S.empty rest)
            (topSort [] visited newVisiting nodeDeps)
      where
        nodeDeps = M.findWithDefault [] node depMap
        newVisiting = S.insert node visiting
        newVisited = S.insert node visited

applySelectiveImports :: [(String, [Ast])] -> Either IncludeError [(String, [Ast])]
applySelectiveImports fileAsts = 
  case validateSymbols fileAsts fileMap of
    Left err -> Left err
    Right _ -> Right $ fmap (filterFile requestMap) fileAsts
  where
    requestMap = M.fromListWith combineRequests
      [ (from, toMaybe items)
      | (_, asts) <- fileAsts
      , ast <- asts
      , (from, items) <- case unwrap ast of
          AInclude f i -> [(f, i)]
          _ -> []
      ]
    
    fileMap = M.fromList fileAsts
    
    toMaybe [] = Nothing
    toMaybe xs = Just xs
    
    filterFile reqMap (fileName, asts) =
      (fileName, maybe astsWithoutIncludes (filterBySymbols astsWithoutIncludes) (M.lookup fileName reqMap))
      where
        astsWithoutIncludes = filter (not . isInclude) asts
        filterBySymbols asts' Nothing = asts'
        filterBySymbols asts' (Just requestedSymbols) =
          filter (symbolIsRequested $ S.fromList requestedSymbols) asts'

validateSymbols :: [(String, [Ast])] -> M.Map String [Ast] -> Either IncludeError ()
validateSymbols fileAsts fileMap =
  case findMissingSymbols fileAsts fileMap of
    [] -> Right ()
    (requester, includedFile, missingSymbol):_ -> Left $ MissingSymbol requester includedFile missingSymbol

findMissingSymbols :: [(String, [Ast])] -> M.Map String [Ast] -> [(String, String, String)]
findMissingSymbols fileAsts fileMap =
  [ (requesterFile, includedFile, symbol)
  | (requesterFile, asts) <- fileAsts
  , ast <- asts
  , (includedFile, requestedSymbols) <- extractIncludeWithSymbols ast
  , not (null requestedSymbols)
  , symbol <- requestedSymbols
  , symbol `notElem` getAvailableSymbols includedFile fileMap
  ]

extractIncludeWithSymbols :: Ast -> [(String, [String])]
extractIncludeWithSymbols ast = case unwrap ast of
  AInclude f i -> [(f, i)]
  _ -> []

getAvailableSymbols :: String -> M.Map String [Ast] -> [String]
getAvailableSymbols fileName fileMap = maybe [] (mapMaybe getSymbolName) (M.lookup fileName fileMap)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []

symbolIsRequested :: S.Set String -> Ast -> Bool
symbolIsRequested symbols = maybe True (`S.member` symbols) . getSymbolName

getSymbolName :: Ast -> Maybe String
getSymbolName ast = case unwrap ast of
  AVarDecl _ name _ -> Just name
  AStruktDef name _ -> Just name
  ATypeAlias name _ -> Just name
  ATraitDef name _ -> Just name
  ATraitImpl trait implType _ -> Just (trait ++ "$" ++ typeToString implType)
  _ -> Nothing

combineRequests :: Maybe [String] -> Maybe [String] -> Maybe [String]
combineRequests Nothing _ = Nothing
combineRequests _ Nothing = Nothing
combineRequests (Just xs) (Just ys) = Just (xs ++ ys)

validateNoDuplicateSymbols :: [(String, [Ast])] -> Either IncludeError ()
validateNoDuplicateSymbols fileAsts =
  case findDuplicateSymbols fileAsts of
    [] -> Right ()
    (symbol, files):_ -> Left $ DuplicateSymbol symbol files

findDuplicateSymbols :: [(String, [Ast])] -> [(String, [String])]
findDuplicateSymbols fileAsts =
  [ (symbol, files)
  | (symbol, files) <- M.toList symbolMap
  , length files > 1
  ]
  where
    symbolMap = M.fromListWith (++) 
      [ (symbol, [fileName])
      | (fileName, asts) <- fileAsts
      , ast <- asts
      , Just symbol <- [getSymbolName ast]
      ]
