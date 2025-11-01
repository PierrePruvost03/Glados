module Compiler.BytecodeGen.Block.Helpers
  ( typesCompatible
  , stripTypeWrappers
  , checkArrayAliasMatch
  , validateInitializerValue
  , compileVarInitialization
  , declareWithValue
  , canInitializeVectorWithDefault
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import Compiler.Type.Checks (bothNumeric, isRefType, isKonst)
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), resolveType, inferType)
import Compiler.Type.Normalization (eqTypeNormalized)
import Compiler.Type.Reference (canInitializeRefWith)
import Compiler.Type.Validation (validateConstantBounds)
import Compiler.BytecodeGen.Utils (extractArraySize)
import Compiler.Unwrap (Unwrappable(..))
import Parser (LineCount)
import qualified Data.Map as M

-- Declare a variable with its value
declareWithValue :: CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv)
declareWithValue env t name exprCode
  | isKonst t' = (exprCode ++ [SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  | otherwise = (exprCode ++ [Alloc, StoreRef, SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  where 
    t' = resolveType env t

-- Check if two types are compatible for assignment/initialization
typesCompatible :: CompilerEnv -> Type -> Type -> Bool
typesCompatible env expected actual =
  eqTypeNormalized expected actual 
  || bothNumeric (stripTypeWrappers expected) (stripTypeWrappers actual) 
  || checkArrayAliasMatch env expected actual
  || checkVectorSizeCompatibility expected actual

-- Check if vector types are compatible (size 0 in expected means dynamic size)
checkVectorSizeCompatibility :: Type -> Type -> Bool
checkVectorSizeCompatibility expected actual =
  case (unwrap (stripTypeWrappers expected), unwrap (stripTypeWrappers actual)) of
    (TVector expectedElemType expectedSizeExpr, TVector actualElemType _) ->
      eqTypeNormalized expectedElemType actualElemType && isZeroSize expectedSizeExpr
    _ -> False
  where
    isZeroSize :: AExpression -> Bool
    isZeroSize sizeExpr = case unwrap sizeExpr of
      AValue val -> case unwrap val of
        ANumber (AInteger 0) -> True
        _ -> False
      _ -> False

-- Check if a type can be initialized with a default value and generate the instructions
canInitializeVectorWithDefault :: Type -> Maybe [Instr]
canInitializeVectorWithDefault t = case unwrap t of
  TVector _ sizeExpr -> case extractArraySize sizeExpr of
    Just size -> Just (replicate size (Push VEmpty) ++ [CreateList size])
    Nothing -> Nothing  -- Dynamic size vectors (size 0) cannot be auto-initialized
  TArray _ sizeExpr -> case extractArraySize sizeExpr of
    Just size -> Just (replicate size (Push VEmpty) ++ [CreateList size])
    Nothing -> Nothing
  _ -> Nothing

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
  -- Check if assigning to an unsigned (Kong) type
  case isUnsignedType t of
    True -> case isNegativeExpression initExpr of
      True -> Left $ IllegalAssignment ("Cannot assign negative value to unsigned type " ++ show t) lineCount
      False -> validateBounds
    False -> validateBounds
  where
    validateBounds = case unwrap initExpr of
      AValue val -> case unwrap val of
        ANumber num -> validateConstantBounds t num lineCount
        _ -> Right ()
      _ -> Right ()

-- Check if a type is an unsigned (Kong) type
isUnsignedType :: Type -> Bool
isUnsignedType t = case unwrap t of
  TKong _ -> True
  TKonst inner -> isUnsignedType inner
  TStrong inner -> isUnsignedType inner
  TRef inner -> isUnsignedType inner
  _ -> False

-- Check if an expression is a negative literal or negation of a positive value
isNegativeExpression :: AExpression -> Bool
isNegativeExpression expr = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger n) -> n < 0
    ANumber (AFloat f) -> f < 0
    _ -> False
  ACall fexp [arg] -> 
    -- Check if it's a unary minus (subtraction with one argument)
    case maybeFuncName fexp of
      Just "-" -> not (isNegativeExpression arg)  -- -(-x) is positive, -(x) is negative if x is positive
      _ -> False
  ACall fexp [lhs, rhs] ->
    -- Check if it's a subtraction that results in negative (heuristic: 0 - positive)
    case maybeFuncName fexp of
      Just "-" -> isZeroExpression lhs && not (isNegativeExpression rhs)
      _ -> False
  _ -> False

-- Check if an expression is zero
isZeroExpression :: AExpression -> Bool
isZeroExpression expr = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger 0) -> True
    ANumber (AFloat 0.0) -> True
    _ -> False
  _ -> False

-- Extract function name from an expression (helper for isNegativeExpression)
maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall name -> Just name
    _ -> Nothing
  _ -> Nothing

-- Compile variable initialization with type checking
compileVarInitialization :: (AExpression -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr])
                         -> Type 
                         -> String 
                         -> AExpression 
                         -> CompilerEnv 
                         -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                         -> LineCount 
                         -> Either CompilerError ([Instr], CompilerEnv)
compileVarInitialization compileExprFn vType vName initExpr env declareFunc lineCount =
  case inferType initExpr newEnv of
    Just inferredType -> 
      compileWithTypeCheck compileExprFn vType vName initExpr newEnv declareFunc resolvedType inferredType lineCount
    Nothing -> 
      compileWithoutTypeCheck compileExprFn vType vName initExpr newEnv declareFunc resolvedType
  where
    resolvedType = resolveType env vType
    newEnv = extendEnvWithVar env vType vName

-- Compile initialization when type is inferred
compileWithTypeCheck :: (AExpression -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr])
                     -> Type -> String -> AExpression -> CompilerEnv
                     -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                     -> Type -> Type -> LineCount
                     -> Either CompilerError ([Instr], CompilerEnv)
compileWithTypeCheck compileExprFn vType vName initExpr env declareFunc resolvedType inferredType lineCount
  | isRefType resolvedType = 
      validateRefInit env resolvedType initExpr inferredType lineCount >>
      compileAndDeclare compileExprFn vType vName initExpr env declareFunc (Just resolvedType)
  | typesCompatible env resolvedType inferredType = 
      compileAndDeclare compileExprFn vType vName initExpr env declareFunc (Just resolvedType)
  | otherwise = 
      typeMismatchError resolvedType inferredType lineCount

-- Compile initialization without type inference
compileWithoutTypeCheck :: (AExpression -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr])
                        -> Type -> String -> AExpression -> CompilerEnv
                        -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                        -> Type
                        -> Either CompilerError ([Instr], CompilerEnv)
compileWithoutTypeCheck compileExprFn vType vName initExpr env declareFunc resolvedType =
  compileAndDeclare compileExprFn vType vName initExpr env declareFunc (Just resolvedType)

-- Compile expression and declare variable
compileAndDeclare :: (AExpression -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr])
                  -> Type -> String -> AExpression -> CompilerEnv
                  -> (CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv))
                  -> Maybe Type
                  -> Either CompilerError ([Instr], CompilerEnv)
compileAndDeclare compileExprFn vType vName initExpr env declareFunc expectedType =
  fmap (declareFunc env vType vName) (compileExprFn initExpr env expectedType)

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
