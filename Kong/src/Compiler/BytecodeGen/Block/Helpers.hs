module Compiler.BytecodeGen.Block.Helpers
  ( typesCompatible
  , stripTypeWrappers
  , checkArrayAliasMatch
  , validateInitializerValue
  , compileVarInitialization
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Checks (bothNumeric, isRefType)
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), resolveType, inferType)
import Compiler.Type.Normalization (eqTypeNormalized)
import Compiler.Type.Reference (canInitializeRefWith)
import Compiler.Type.Validation (validateConstantBounds)
import Compiler.Unwrap (Unwrappable(..))
import Parser (LineCount)
import qualified Data.Map as M

-- Check if two types are compatible for assignment/initialization
typesCompatible :: CompilerEnv -> Type -> Type -> Bool
typesCompatible env expected actual =
  eqTypeNormalized expected actual 
  || bothNumeric (stripTypeWrappers expected) (stripTypeWrappers actual) 
  || checkArrayAliasMatch env expected actual

-- Remove type wrappers (TKonst, TStrong, TKong) from a type
stripTypeWrappers :: Type -> Type
stripTypeWrappers t = case unwrap t of
  TKonst x -> stripTypeWrappers x
  TStrong x -> stripTypeWrappers x
  TKong x -> stripTypeWrappers x
  _ -> t

-- Check if a TVector type matches a TArray type (for array aliases)
checkArrayAliasMatch :: CompilerEnv -> Type -> Type -> Bool
checkArrayAliasMatch env expected actual =
  case (unwrap (stripTypeWrappers expected), unwrap (stripTypeWrappers actual)) of
    (TVector customT sizeExpr, TArray eltT _) ->
      checkCustomArrayType env customT sizeExpr eltT
    _ -> False

-- Helper to check if custom type matches array element type
checkCustomArrayType :: CompilerEnv -> Type -> AExpression -> Type -> Bool
checkCustomArrayType env customT sizeExpr eltT =
  case unwrap customT of
    TCustom "Array" -> checkSizeExprMatchesType env sizeExpr eltT
    _ -> False

-- Helper to check if size expression's type name matches element type
checkSizeExprMatchesType :: CompilerEnv -> AExpression -> Type -> Bool
checkSizeExprMatchesType env sizeExpr eltT =
  case unwrap sizeExpr of
    AValue val -> case unwrap val of
      AVarCall typeName ->
        eqTypeNormalized (resolveType env (fst eltT, TCustom typeName)) eltT
      _ -> False
    _ -> False

-- Validate initializer value for constant bounds checking
validateInitializerValue :: Type -> AExpression -> LineCount -> Either CompilerError ()
validateInitializerValue t initExpr lineCount =
  case unwrap initExpr of
    AValue val -> case unwrap val of
      ANumber num -> validateConstantBounds t num lineCount
      _ -> Right ()
    _ -> Right ()

-- Compile variable initialization with type checking
compileVarInitialization :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
                         -> Type 
                         -> String 
                         -> AExpression 
                         -> CompilerEnv 
                         -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                         -> LineCount 
                         -> Either CompilerError ([Instr], CompilerEnv)
compileVarInitialization compileExpr vType vName initExpr env declareWithValue lineCount =
  case inferType initExpr newEnv of
    Just inferredType -> 
      compileWithTypeCheck compileExpr vType vName initExpr newEnv declareWithValue resolvedType inferredType lineCount
    Nothing -> 
      compileWithoutTypeCheck compileExpr vType vName initExpr newEnv declareWithValue
  where
    resolvedType = resolveType env vType
    newEnv = extendEnvWithVar env vType vName

-- Compile initialization when type is inferred
compileWithTypeCheck :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
                     -> Type -> String -> AExpression -> CompilerEnv
                     -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                     -> Type -> Type -> LineCount
                     -> Either CompilerError ([Instr], CompilerEnv)
compileWithTypeCheck compileExpr vType vName initExpr env declareWithValue resolvedType inferredType lineCount
  | isRefType resolvedType = 
      validateRefInit env resolvedType initExpr inferredType lineCount >>
      compileAndDeclare compileExpr vType vName initExpr env declareWithValue
  | typesCompatible env resolvedType inferredType = 
      compileAndDeclare compileExpr vType vName initExpr env declareWithValue
  | otherwise = 
      typeMismatchError resolvedType inferredType lineCount

-- Compile initialization without type inference
compileWithoutTypeCheck :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
                        -> Type -> String -> AExpression -> CompilerEnv
                        -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                        -> Either CompilerError ([Instr], CompilerEnv)
compileWithoutTypeCheck compileExpr vType vName initExpr env declareWithValue =
  compileAndDeclare compileExpr vType vName initExpr env declareWithValue

-- Compile expression and declare variable
compileAndDeclare :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
                  -> Type -> String -> AExpression -> CompilerEnv
                  -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                  -> Either CompilerError ([Instr], CompilerEnv)
compileAndDeclare compileExpr vType vName initExpr env declareWithValue =
  fmap (declareWithValue env vType vName) (compileExpr initExpr env)

-- Extend environment with a new variable binding
extendEnvWithVar :: CompilerEnv -> Type -> String -> CompilerEnv
extendEnvWithVar env t name = env { typeAliases = M.insert name t (typeAliases env) }

-- Validate that a reference can be initialized with the given expression
validateRefInit :: CompilerEnv -> Type -> AExpression -> Type -> LineCount -> Either CompilerError ()
validateRefInit env refType initExpr inferredType lineCount =
  canInitializeRefWith env refType initExpr >>= \canInit ->
    case canInit of
      True -> Right ()
      False -> refInitError refType inferredType lineCount

typeMismatchError :: Type -> Type -> LineCount -> Either CompilerError a
typeMismatchError expected actual lineCount =
  Left $ InvalidArguments ("Initializer type mismatch: expected " ++ show expected ++ ", got " ++ show actual) lineCount

refInitError :: Type -> Type -> LineCount -> Either CompilerError a
refInitError refType inferredType lineCount =
  Left $ InvalidArguments ("Cannot initialize reference of type " ++ show refType ++ " with expression of type " ++ show inferredType) lineCount
