module Compiler.Type.Normalization
  ( stripWrap
  , eqTypeNormalized
  , normalize
  , normalizeExpr
  , normalizeValue
  , normalizeAccess
  ) where

import DataStruct.Ast
import Compiler.Unwrap (Unwrappable(..))

-- Strip Konst, Strong, Kong wrappers from a type
stripWrap :: Type -> Type
stripWrap t = case unwrap t of
  TKonst ty -> stripWrap ty
  TStrong ty -> stripWrap ty
  TKong ty -> stripWrap ty
  _ -> t

-- Normalize a type by removing line counts and stripping wrappers
normalize :: Type -> Type
normalize t = case unwrap t of
  TKonst ty -> normalize ty
  TStrong ty -> normalize ty
  TKong ty -> normalize ty
  TRef ty -> ((0, 0), TRef (normalize ty))
  TArray ty e -> ((0, 0), TArray (normalize ty) (normalizeExpr e))
  TVector ty e -> ((0, 0), TVector (normalize ty) (normalizeExpr e))
  TTuple tys -> ((0, 0), TTuple (map normalize tys))
  TFunc args ret -> ((0, 0), TFunc (map normalize args) (normalize ret))
  raw -> ((0, 0), raw)

normalizeExpr :: AExpression -> AExpression
normalizeExpr expr = case unwrap expr of
  AValue val -> ((0, 0), AValue (normalizeValue val))
  AAccess acc -> ((0, 0), AAccess (normalizeAccess acc))
  ACall fexp args -> ((0, 0), ACall (normalizeExpr fexp) (map normalizeExpr args))
  ACast ty ex -> ((0, 0), ACast (normalize ty) (normalizeExpr ex))
  AAttribution var rhs -> ((0, 0), AAttribution var (normalizeExpr rhs))
  AMethodCall obj method args -> ((0, 0), AMethodCall (normalizeExpr obj) method (map normalizeExpr args))

normalizeValue :: AstValue -> AstValue
normalizeValue val = case unwrap val of
  ANumber n -> ((0, 0), ANumber n)
  AString s -> ((0, 0), AString s)
  ATuple exprs -> ((0, 0), ATuple (map normalizeExpr exprs))
  AArray exprs -> ((0, 0), AArray (map normalizeExpr exprs))
  AVector exprs -> ((0, 0), AVector (map normalizeExpr exprs))
  AStruct flds -> ((0, 0), AStruct (map (\(n, e) -> (n, normalizeExpr e)) flds))
  ALambda params ret body -> ((0, 0), ALambda params ret body)
  AVarCall v -> ((0, 0), AVarCall v)

normalizeAccess :: AstAccess -> AstAccess
normalizeAccess acc = case unwrap acc of
  AArrayAccess arr idx -> ((0, 0), AArrayAccess (normalizeExpr arr) (normalizeExpr idx))
  AVectorAccess vec idx -> ((0, 0), AVectorAccess (normalizeExpr vec) (normalizeExpr idx))
  ATupleAccess tup idx -> ((0, 0), ATupleAccess (normalizeExpr tup) (normalizeExpr idx))
  AStructAccess str flds -> ((0, 0), AStructAccess (normalizeExpr str) flds)

-- Check if two types are equal after normalization
eqTypeNormalized :: Type -> Type -> Bool
eqTypeNormalized a b = normalize a == normalize b
