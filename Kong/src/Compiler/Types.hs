module Compiler.Types
  ( CompilerError(..)
  , ProgramError(..)
  , CompilerEnv (..)
  , emptyEnv
  , insertTypeAlias
  , resolveType
  , checkComparisonTypes
  , eqTypeNormalized
  , checkFunctionCallTypes
  , checkAssignmentType
  , getFunctionArgTypes
  , typesEqual
  , numericCompatible
  , isFloatType
  , isKonst
  , inferType
  , comparisonOps
  , arithOps
  , bothNumeric
  ) where

import DataStruct.Ast
import qualified Data.Map as M
import Compiler.TypeError (TypeError(..))

maybeFuncName :: AExpression -> Maybe String
maybeFuncName (AValue (AVarCall n)) = Just n
maybeFuncName _ = Nothing

data CompilerEnv = CompilerEnv
  { typeAliases :: M.Map String Type
  , structDefs  :: M.Map String [(Type, String)]
  }

emptyEnv :: CompilerEnv
emptyEnv = CompilerEnv M.empty M.empty

data CompilerError
  = UnsupportedAst String
  | IllegalAssignment String
  | UnknownVariable String
  | UnknownFunction String
  | InvalidArguments String
  | MissingMainFunction String
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)


insertTypeAlias :: CompilerEnv -> Ast -> CompilerEnv
insertTypeAlias env (ATypeAlias name ty) = env { typeAliases = M.insert name ty (typeAliases env) }
insertTypeAlias env (AStruktDef name fds) = env { structDefs = M.insert name fds (structDefs env) }
insertTypeAlias env _ = env

resolveType :: CompilerEnv -> Type -> Type
resolveType env t@(TCustom name) =
  case M.lookup name (typeAliases env) of
    Just realTy -> resolveType env realTy
    Nothing -> case M.lookup name (structDefs env) of
      Just _ -> TStruct name
      Nothing -> t
resolveType env (TKonst ty) = TKonst (resolveType env ty)
resolveType env (TStrong ty) = TStrong (resolveType env ty)
resolveType env (TKong ty) = TKong (resolveType env ty)
resolveType _ (TStruct s) = TStruct s
resolveType _ (TTrait s) = TTrait s
resolveType env (TArray ty e) = TArray (resolveType env ty) e
resolveType env (TVector ty e) = TVector (resolveType env ty) e
resolveType env (TTuple tys) = TTuple (map (resolveType env) tys)
resolveType env (TFunc args ret) = TFunc (map (resolveType env) args) (resolveType env ret)
resolveType _ t = t

checkComparisonTypes :: Type -> Type -> Either TypeError ()
checkComparisonTypes t1 t2
  | eqTypeNormalized t1 t2 = Right ()
  | bothNumeric (stripWrap t1) (stripWrap t2) = Right ()
  | otherwise = Left $ InvalidComparison (show t1) (show t2)

stripWrap :: Type -> Type
stripWrap (TKonst t) = stripWrap t
stripWrap (TStrong t) = stripWrap t
stripWrap (TKong t) = stripWrap t
stripWrap t = t

bothNumeric :: Type -> Type -> Bool
bothNumeric TInt TInt = True
bothNumeric TFloat TFloat = True
bothNumeric TInt TFloat = True
bothNumeric TFloat TInt = True
bothNumeric _ _ = False

eqTypeNormalized :: Type -> Type -> Bool
eqTypeNormalized a b = normalize a == normalize b

normalize :: Type -> Type
normalize (TKonst t) = normalize t
normalize (TStrong t) = normalize t
normalize (TKong t) = normalize t
normalize (TArray t _) = TArray (normalize t) (AValue (ANumber (AInteger 0)))
normalize (TVector t _) = TVector (normalize t) (AValue (ANumber (AInteger 0)))
normalize (TTuple ts) = TTuple (map normalize ts)
normalize t = t

checkFunctionCallTypes :: [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes (t:ts) (Just a:as)
  | typesEqual t a || numericCompatible t a = checkFunctionCallTypes ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a)
checkFunctionCallTypes [] [] = Right ()
checkFunctionCallTypes _ _ = Left $ InvalidArguments "Function argument count or type mismatch"

getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just (TKonst (TTuple ts)) -> case ts of
      [] -> Just []
      _  -> Just (init ts)
    Just (TKonst (TFunc args _)) -> Just args
    Just (TFunc args _) -> Just args
    _ -> Nothing

getFunctionReturnType :: M.Map String Type -> String -> Maybe Type
getFunctionReturnType envMap fname =
  case M.lookup fname envMap of
    Just (TKonst (TTuple ts)) -> case ts of
      [] -> Nothing
      _  -> Just (last ts)
    Just (TKonst (TFunc _ ret)) -> Just ret
    Just (TFunc _ ret) -> Just ret
    _ -> Nothing

checkAssignmentType :: Maybe Type -> Maybe Type -> Either CompilerError ()
checkAssignmentType (Just expected) (Just actual)
  | eqTypeNormalized expected actual = Right ()
  | otherwise = Left $ IllegalAssignment ("Type mismatch on assignment: expected " ++ show expected ++ ", got " ++ show actual)
checkAssignmentType _ _ = Left $ IllegalAssignment "Unable to infer types for assignment"

typesEqual :: Type -> Type -> Bool
typesEqual = eqTypeNormalized

numericCompatible :: Type -> Type -> Bool
numericCompatible a b =
  case (stripWrap a, stripWrap b) of
    (TInt, TInt) -> True
    (TFloat, TFloat) -> True
    (TInt, TFloat) -> True
    (TFloat, TInt) -> True
    _ -> False

isFloatType :: Type -> Bool
isFloatType t = case stripWrap t of
  TFloat -> True
  _ -> False

isKonst :: Type -> Bool
isKonst (TKonst _) = True
isKonst _ = False

inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType (AValue (ANumber (AInteger _))) _ = Just TInt
inferType (AValue (ANumber (AFloat _))) _ = Just TFloat
inferType (AValue (ANumber (ABool _))) _ = Just TBool
inferType (AValue (ANumber (AChar _))) _ = Just TChar
inferType (AValue (AString _)) _ = Just TString
inferType (AValue (ALambda _ retType _)) _ = Just retType
inferType (AValue (ATuple exprs)) env =
  case traverse (\e -> inferType e env) exprs of
    Just ts -> Just (TTuple ts)
    Nothing -> Nothing
inferType (AValue (AArray exprs)) env =
  inferHomogeneousList TArray exprs env
inferType (AValue (AVector exprs)) env =
  inferHomogeneousList TVector exprs env
inferType (AValue (AStruct _)) _ = Nothing
inferType (AValue (AVarCall v)) env = M.lookup v (typeAliases env)
inferType (AAttribution _ _) _ = Nothing
inferType (AAccess acc) env = inferAccessType acc env
inferType (ACall fexp [l, r]) env | maybeFuncName fexp `elem` map Just arithOps =
  case (inferType l env, inferType r env) of
    (Just t1, Just t2)
      | numericCompatible t1 t2 && (isFloatType t1 || isFloatType t2) -> Just TFloat
      | numericCompatible t1 t2 -> Just TInt
    _ -> Nothing
inferType (ACall fexp [_l, _r]) _env | maybeFuncName fexp `elem` map Just comparisonOps = Just TBool
inferType (ACall fexp _) env
  | maybeFuncName fexp `elem` map Just (comparisonOps ++ ["print"]) = Nothing
  | Just name <- maybeFuncName fexp = getFunctionReturnType (typeAliases env) name
  | otherwise = Nothing
inferType (AMethodCall _ _ _) _ = Nothing

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/"]

inferAccessType :: AstAccess -> CompilerEnv -> Maybe Type
inferAccessType (AArrayAccess arrExpr _) env =
  case inferType arrExpr env of
    Just t -> case stripWrap (resolveType env t) of
      TArray et _ -> Just et
      TVector et _ -> Just et
      _ -> Nothing
    Nothing -> Nothing
inferAccessType (AVectorAccess vecExpr _) env =
  case inferType vecExpr env of
    Just t -> case stripWrap (resolveType env t) of
      TVector et _ -> Just et
      TArray et _ -> Just et
      _ -> Nothing
    Nothing -> Nothing
inferAccessType (ATupleAccess tupleExpr idxExpr) env =
  case inferType tupleExpr env of
    Just t -> case stripWrap (resolveType env t) of
      TTuple ts -> tupleIndexType' ts idxExpr
      _ -> Nothing
    Nothing -> Nothing
inferAccessType (AStructAccess structExpr fields) env =
  case inferType structExpr env of
    Just t0 -> go t0 fields
    Nothing -> Nothing
  where
    go t [] = Just (resolveType env t)
    go t (f:fs) =
      case stripWrap (resolveType env t) of
        TStruct sname ->
          case M.lookup sname (structDefs env) of
            Just fds ->
              case lookup f (map (\(ty, nm) -> (nm, ty)) fds) of
                Just fty -> go fty fs
                Nothing -> Nothing
            Nothing -> Nothing
        _ -> Nothing

tupleIndexType' :: [Type] -> AExpression -> Maybe Type
tupleIndexType' ts (AValue (ANumber (AInteger i)))
  | i >= 0 && i < length ts = Just (ts !! i)
  | otherwise = Nothing
tupleIndexType' _ _ = Nothing

inferHomogeneousList :: (Type -> AExpression -> Type) -> [AExpression] -> CompilerEnv -> Maybe Type
inferHomogeneousList ctor exprs env =
  case traverse (\e -> inferType e env) exprs of
    Just [] -> Just (ctor TInt (AValue (ANumber (AInteger 0))))
    Just (t:ts) | all (typesEqual t) ts -> Just (ctor t (AValue (ANumber (AInteger (length exprs)))))
    _ -> Nothing
