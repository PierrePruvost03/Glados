module Compiler.Type.Reference
  ( checkReferenceValidity
  , checkDereferenceValidity
  , validateReferences
  , canInitializeRefWith
  ) where

import DataStruct.Ast
import Parser (LineCount)
import qualified Data.Map as M
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.Type.Normalization (stripWrap, eqTypeNormalized)
import Compiler.Type.Checks (isLValue, isTemporaryValue, isRefType, extractRefType)
import Compiler.Type.Inference (CompilerEnv(..), inferType, resolveType, getFunctionArgTypes)
import Compiler.Type.Error (CompilerError(..))

-- Extract function name from expression if it's a variable call
maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall n -> Just n
    _ -> Nothing
  _ -> Nothing

-- Check that an expression can be used to create a reference (must be lvalue, not temporary)
checkReferenceValidity :: AExpression -> LineCount -> Either CompilerError ()
checkReferenceValidity expr lineCount
  | isTemporaryValue expr = 
      Left $ ReferenceToTemporary 
        ("Cannot create reference to temporary value: " ++ show expr) lineCount
  | not (isLValue expr) = 
      Left $ InvalidReference 
        ("Cannot create reference to non-lvalue expression: " ++ show expr) lineCount
  | otherwise = Right ()

-- Check if dereferencing is valid (expression must be of reference type)
checkDereferenceValidity :: AExpression -> Type -> LineCount -> Either CompilerError ()
checkDereferenceValidity _ exprType lineCount = 
  case unwrap (stripWrap exprType) of
    TRef _ -> Right ()
    _ -> Left $ InvalidReference 
           ("Attempting to dereference non-reference type: " ++ show exprType) lineCount

-- Recursively validate all reference uses in an expression (references must point to valid lvalues)
validateReferences :: AExpression -> CompilerEnv -> M.Map String Type -> Either CompilerError ()
validateReferences expr env typeEnv = case unwrap expr of
  ACast targetType innerExpr ->
    validateReferences innerExpr env typeEnv >>= \() ->
      case unwrap (stripWrap (resolveType env targetType)) of
        TRef _ -> checkReferenceValidity innerExpr (lc expr)
        _ -> Right ()
  ACall funcExpr args ->
    validateReferences funcExpr env typeEnv >>= \() ->
      mapM_ (\arg -> validateReferences arg env typeEnv) args >>= \() ->
        case maybeFuncName funcExpr of
          Just fname -> case getFunctionArgTypes typeEnv fname of
            Just argTypes -> validateRefArgs args argTypes (lc expr)
            Nothing -> Right ()
          Nothing -> Right ()
  AAccess acc -> validateAccessReferences acc env typeEnv
  AValue val -> case unwrap val of
    ATuple exprs -> mapM_ (\e -> validateReferences e env typeEnv) exprs
    AArray exprs -> mapM_ (\e -> validateReferences e env typeEnv) exprs
    AVector exprs -> mapM_ (\e -> validateReferences e env typeEnv) exprs
    AStruct structFields -> mapM_ (\(_, e) -> validateReferences e env typeEnv) structFields
    _ -> Right ()
  AAttribution _ rhs -> validateReferences rhs env typeEnv
  AMethodCall obj _ args ->
    validateReferences obj env typeEnv >>= \() ->
      mapM_ (\arg -> validateReferences arg env typeEnv) args

-- Validate that function arguments respect reference constraints (ref params need lvalue args)
validateRefArgs :: [AExpression] -> [Type] -> LineCount -> Either CompilerError ()
validateRefArgs [] [] _ = Right ()
validateRefArgs (arg:args) (t:ts) lineCount =
  case unwrap (stripWrap t) of
    TRef _ -> checkReferenceValidity arg lineCount >>= \() -> validateRefArgs args ts lineCount
    _ -> validateRefArgs args ts lineCount
validateRefArgs _ _ _ = Right ()

-- Recursively validate references in access expressions (array, tuple, struct)
validateAccessReferences :: AstAccess -> CompilerEnv -> M.Map String Type -> Either CompilerError ()
validateAccessReferences acc env typeEnv = case unwrap acc of
  AArrayAccess arrExpr idxExpr ->
    validateReferences arrExpr env typeEnv >>= \() ->
      validateReferences idxExpr env typeEnv
  AVectorAccess vecExpr idxExpr ->
    validateReferences vecExpr env typeEnv >>= \() ->
      validateReferences idxExpr env typeEnv
  ATupleAccess tupExpr idxExpr ->
    validateReferences tupExpr env typeEnv >>= \() ->
      validateReferences idxExpr env typeEnv
  AStructAccess structExpr _ -> 
    validateReferences structExpr env typeEnv

-- Check if an expression can be used to initialize a reference of a given type (validates both type compatibility and lvalue status)
canInitializeRefWith :: CompilerEnv -> Type -> AExpression -> Either CompilerError Bool
canInitializeRefWith env refType expr
  | not (isRefType refType) = Right False
  | otherwise = 
      case checkReferenceValidity expr (lc expr) of
        Left err -> Left err
        Right () -> case inferType expr env of
          Just exprType -> Right (eqTypeNormalized (extractRefType refType) exprType)
          Nothing -> Right False
