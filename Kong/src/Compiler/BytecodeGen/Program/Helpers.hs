module Compiler.BytecodeGen.Program.Helpers
  ( ensureMainExists
  , hasMainFunction
  , validateMainSignature
  , buildAliasEnvironment
  , processAstForEnv
  , extractAllTopLevel
  , enrichEnvironmentWithAst
  , extractAllTopLevelAsts
  , buildSimpleAliasEnv
  , expand
  , compilePairWithEnv
  , resultsToEither
  , compileWithEnv
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (ProgramError(..), CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), emptyEnv, insertInEnv)
import Compiler.Type.Return (checkMainSignature)
import Compiler.Type.Validation (validateStructDefinition, validateNoDuplicateDeclaration, validateNoDuplicateStruct)
import Compiler.Type.Normalization (typeToString)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.BytecodeGen.Statements (compileAst)
import qualified Data.Map as M

-- Check if main function exists and validate its signature
ensureMainExists :: [Instr] -> CompilerEnv -> Either [ProgramError] [Instr]
ensureMainExists instrs env
  | not (hasMainFunction instrs) = mainMissingError
  | otherwise = validateMainSignature instrs env

-- Check if instructions contain a main function definition
hasMainFunction :: [Instr] -> Bool
hasMainFunction = any isMainSetVar
  where
    isMainSetVar (SetVar name) = name == "main"
    isMainSetVar _ = False

-- Validate main function signature and append call instructions
validateMainSignature :: [Instr] -> CompilerEnv -> Either [ProgramError] [Instr]
validateMainSignature instrs env =
  case M.lookup "main" (typeAliases env) of
    Nothing -> mainMissingError
    Just mainType -> 
      case checkMainSignature mainType ((0, 0)) of
        Right () -> Right (instrs ++ [PushEnv "main", Call, Ret])
        Left err -> Left [ProgramError "<global>" ((0, 0), ABlock []) err]

-- Generate missing main function error
mainMissingError :: Either [ProgramError] a
mainMissingError = Left [ProgramError "<global>" ((0, 0), ABlock []) MissingMainFunction]

-- Build alias environment from AST list
buildAliasEnvironment :: [Ast] -> Either [ProgramError] CompilerEnv
buildAliasEnvironment asts =
  case foldl processAstForEnv (emptyEnv, []) (extractAllTopLevel asts) of
    (env, []) -> Right env
    (_, errs) -> Left errs

-- Validate trait implementation methods match trait definition
validateTraitImpl :: String -> Type -> [Ast] -> CompilerEnv -> (Int, Int) -> Either CompilerError ()
validateTraitImpl traitName implType methods env lnCount =
  case M.lookup traitName (traitDefs env) of
    Nothing -> Left (UndefinedTrait traitName lnCount)
    Just expectedMethods ->
      case (findMissingMethods expectedMethods implMethods, findUnexpectedMethods expectedMethods implMethods) of
        ([], []) -> Right ()
        (missing:_, _) -> Left (MissingTraitMethod traitName (typeToString implType) missing lnCount)
        (_, unexpected:_) -> Left (UnexpectedTraitMethod traitName (typeToString implType) unexpected lnCount)
  where
    implMethods = [methodName | method <- methods, AVarDecl _ fullName _ <- [unwrap method], Just methodName <- [extractMethodName fullName]]
    extractMethodName name = case dropWhile (/= '$') name of
      '$':rest -> Just rest
      _ -> Nothing
    traitMethodNames expected = [name | (name, _, _) <- expected]
    findMissingMethods expected impl = [m | m <- traitMethodNames expected, m `notElem` impl]
    findUnexpectedMethods expected impl = [m | m <- impl, m `notElem` traitMethodNames expected]

-- Process a single AST node to build environment
processAstForEnv :: (CompilerEnv, [ProgramError]) -> Ast -> (CompilerEnv, [ProgramError])
processAstForEnv (env, errs) ast = case unwrap ast of
  ATypeAlias name _ -> 
    handleValidation env errs ast (validateNoDuplicateDeclaration name env (lc ast))
  AStruktDef name fds -> 
    handleStructDef env errs ast name fds
  ATraitDef name methods ->
    (insertInEnv env ast, errs)
  ATraitImpl traitName implType methods ->
    case validateTraitImpl traitName implType methods env (lc ast) of
      Right () -> (insertInEnv env ast, errs)
      Left err -> (env, errs ++ [ProgramError "<global>" ast err])
  AVarDecl _ name _ ->
    case validateNoDuplicateDeclaration name env (lc ast) of
      Right () -> (insertInEnv env ast, errs)
      Left err -> (env, errs ++ [ProgramError "<global>" ast err])
  _ -> (env, errs)

-- Handle validation result and update environment
handleValidation :: CompilerEnv -> [ProgramError] -> Ast -> Either CompilerError () -> (CompilerEnv, [ProgramError])
handleValidation env errs ast validationResult =
  case validationResult of
    Right () -> (insertInEnv env ast, errs)
    Left err -> (env, errs ++ [ProgramError "<global>" ast err])

-- Handle struct definition
handleStructDef :: CompilerEnv -> [ProgramError] -> Ast -> String -> [(Type, String)] -> (CompilerEnv, [ProgramError])
handleStructDef env errs ast name fds =
  case validateNoDuplicateStruct name env (lc ast) of
    Left err -> (env, errs ++ [ProgramError "<global>" ast err])
    Right () -> 
      case validateStructDefinition env name fds (lc ast) of
        Right () -> (insertInEnv env ast, errs)
        Left err -> (env, errs ++ [ProgramError "<global>" ast err])

-- Extract all top level declarations from AST list
extractAllTopLevel :: [Ast] -> [Ast]
extractAllTopLevel = concatMap extractFromAst

-- Recursively extract top level declarations from a single AST
extractFromAst :: Ast -> [Ast]
extractFromAst ast = case unwrap ast of
  ABlock innerAsts -> concatMap extractFromAst innerAsts
  ATraitImpl _ _ methods -> ast : methods
  _ -> [ast]

-- Alias for extractAllTopLevel (for backwards compatibility)
extractAllTopLevelAsts :: [Ast] -> [Ast]
extractAllTopLevelAsts = extractAllTopLevel

-- Enrich environment with a single AST (adds variable types to typeAliases)
enrichEnvironmentWithAst :: CompilerEnv -> Ast -> CompilerEnv
enrichEnvironmentWithAst env ast = case unwrap ast of
  AVarDecl t name _ -> env { typeAliases = M.insert name t (typeAliases env) }
  ATraitDef name methods -> env { traitDefs = M.insert name methods (traitDefs env) }
  ATraitImpl traitName implType _ ->
    env { traitImpls = M.insert 
            (traitName, typeToString implType) 
            (maybe [] (map (\(n, _, _) -> n)) (M.lookup traitName (traitDefs env))) 
            (traitImpls env) }
  _ -> env

-- Simple alias building for use with foldl (no error handling)
buildSimpleAliasEnv :: CompilerEnv -> Ast -> CompilerEnv
buildSimpleAliasEnv env ast = case unwrap ast of
  ATypeAlias name typ -> env { typeAliases = M.insert name typ (typeAliases env) }
  AStruktDef name fds -> env { structDefs = M.insert name fds (structDefs env) }
  ATraitDef name methods -> env { traitDefs = M.insert name methods (traitDefs env) }
  ATraitImpl traitName implType _ ->
    env { traitImpls = M.insert 
            (traitName, typeToString implType) 
            (maybe [] (map (\(n, _, _) -> n)) (M.lookup traitName (traitDefs env))) 
            (traitImpls env) }
  AVarDecl t name _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

-- Expand file-AST pairs into individual file-AST tuples
expand :: [(String, [Ast])] -> [(String, Ast)]
expand pairs = [(file, ast) | (file, asts) <- pairs, ast <- asts]

-- Compile a file-AST pair with environment
compilePairWithEnv :: CompilerEnv -> (String, Ast) -> Either ProgramError [Instr]
compilePairWithEnv env (f, a) =
  either (Left . ProgramError f a) Right (compileWithEnv env a)

-- Compile AST with environment, extracting only instructions
compileWithEnv :: CompilerEnv -> Ast -> Either CompilerError [Instr]
compileWithEnv env ast = fst <$> compileAst ast env

-- Fold multiple results into a single Either with accumulated errors
resultsToEither :: [Either ProgramError [Instr]] -> Either [ProgramError] [Instr]
resultsToEither = foldr step (Right [])
  where
    step (Left e) (Left es) = Left (e : es)
    step (Left e) (Right _) = Left [e]
    step (Right _) (Left es) = Left es
    step (Right is) (Right js) = Right (is ++ js)
