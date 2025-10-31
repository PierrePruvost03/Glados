module Compiler.Type.Checks
  ( isFloatType
  , isKonst
  , isRefType
  , extractRefType
  , bothNumeric
  , numericCompatible
  , isLValue
  , isTemporaryValue
  , comparisonOps
  , arithOps
  , checkComparisonTypes
  , isNonComparableType
  ) where

import DataStruct.Ast
import Parser (LineCount)
import Compiler.Unwrap (Unwrappable(..))
import Compiler.Type.Normalization (stripWrap, eqTypeNormalized)
import Compiler.Type.Error (CompilerError(..))

isFloatType :: Type -> Bool
isFloatType t = case unwrap (stripWrap t) of
  TFloat -> True
  _ -> False

isKonst :: Type -> Bool
isKonst t = case unwrap t of
  TKonst _ -> True
  _ -> False

isRefType :: Type -> Bool
isRefType t = case unwrap t of
  TRef _ -> True
  TKonst ty -> isRefType ty
  TStrong ty -> isRefType ty
  TKong ty -> isRefType ty
  _ -> False

-- Extract the inner type from a reference
extractRefType :: Type -> Type
extractRefType t = case unwrap t of
  TRef ty -> ty
  TKonst ty -> extractRefType ty
  TStrong ty -> extractRefType ty
  TKong ty -> extractRefType ty
  _ -> t

-- Check if both types are numeric (Int or Float)
bothNumeric :: Type -> Type -> Bool
bothNumeric t1 t2 = case (unwrap t1, unwrap t2) of
  (TInt, TInt) -> True
  (TFloat, TFloat) -> True
  (TInt, TFloat) -> True
  (TFloat, TInt) -> True
  _ -> False

-- Check if types are compatible for numeric operations
numericCompatible :: Type -> Type -> Bool
numericCompatible a b =
  case (unwrap (stripWrap a), unwrap (stripWrap b)) of
    (t1, t2) | isOperableType t1 && isOperableType t2 -> True
    _ -> False
  where
    isOperableType t = case t of
      TInt -> True
      TFloat -> True
      TChar -> True
      TBool -> True
      _ -> False

-- Check if an expression is an lvalue (can be assigned to)
isLValue :: AExpression -> Bool
isLValue expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall _ -> True
    _ -> False
  AAccess acc -> case unwrap acc of
    AArrayAccess _ _ -> True
    AVectorAccess _ _ -> True
    ATupleAccess _ _ -> True
    AStructAccess _ _ -> True
  _ -> False

isTemporaryValue :: AExpression -> Bool
isTemporaryValue expr = case unwrap expr of
  ACall _ _ -> True
  ACast _ _ -> True
  AValue val -> case unwrap val of
    ANumber _ -> True
    AString _ -> True
    AStruct _ -> True
    AArray _ -> True
    AVector _ -> True
    ATuple _ -> True
    ALambda _ _ _ -> True
    AVarCall _ -> False
  AAccess _ -> False
  AAttribution _ _ -> False
  AMethodCall _ _ _ -> True

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/", "%"]

-- Check if two types can be compared (must be equal or both numeric, and not non-comparable)
checkComparisonTypes :: Type -> Type -> LineCount -> Either CompilerError ()
checkComparisonTypes t1 t2 lc
  | eqTypeNormalized t1 t2 = Right ()
  | bothNumeric (stripWrap t1) (stripWrap t2) = Right ()
  | otherwise = Left $ InvalidComparison t1 t2 lc

-- Check if a type cannot be compared (arrays, structs, tuples are non-comparable)
isNonComparableType :: Type -> Bool
isNonComparableType t = case unwrap t of
  TArray _ _ -> True
  TStruct _ -> True
  TTuple _ -> True
  TKonst _ -> True
  _ -> False
