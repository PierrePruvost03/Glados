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
  , unwrapAst
  , unwrapExpr
  , unwrapType
  , unwrapAccess
  , unwrapValue
  , getAstLineCount
  , getExprLineCount
  , getTypeLineCount
  , getAccessLineCount
  , getValueLineCount
  ) where

import DataStruct.Ast
import Parser (LineCount)
import qualified Data.Map as M
import Compiler.TypeError (TypeError(..))

maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    AVarCall n -> Just n
    _ -> Nothing
  _ -> Nothing

data CompilerEnv = CompilerEnv
  { typeAliases :: M.Map String Type
  , structDefs  :: M.Map String [(Type, String)]
  }

emptyEnv :: CompilerEnv
emptyEnv = CompilerEnv M.empty M.empty

data CompilerError
  = UnsupportedAst String LineCount
  | IllegalAssignment String LineCount
  | UnknownVariable String LineCount
  | UnknownFunction String LineCount
  | InvalidArguments String LineCount
  | MissingMainFunction String
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)


insertTypeAlias :: CompilerEnv -> Ast -> CompilerEnv
insertTypeAlias env ast = case unwrapAst ast of
  ATypeAlias name ty -> env { typeAliases = M.insert name ty (typeAliases env) }
  AStruktDef name fds -> env { structDefs = M.insert name fds (structDefs env) }
  _ -> env

resolveType :: CompilerEnv -> Type -> Type
resolveType env t = case unwrapType t of
  TCustom name ->
    case M.lookup name (typeAliases env) of
      Just realTy -> resolveType env realTy
      Nothing -> case M.lookup name (structDefs env) of
        Just _ -> (fst t, TStruct name)
        Nothing -> t
  TKonst ty -> (fst t, TKonst (resolveType env ty))
  TStrong ty -> (fst t, TStrong (resolveType env ty))
  TKong ty -> (fst t, TKong (resolveType env ty))
  TRef ty -> (fst t, TRef (resolveType env ty))
  TStruct s -> (fst t, TStruct s)
  TTrait s -> (fst t, TTrait s)
  TArray ty e -> (fst t, TArray (resolveType env ty) e)
  TVector ty e -> (fst t, TVector (resolveType env ty) e)
  TTuple tys -> (fst t, TTuple (map (resolveType env) tys))
  TFunc args ret -> (fst t, TFunc (map (resolveType env) args) (resolveType env ret))
  raw -> (fst t, raw)

checkComparisonTypes :: Type -> Type -> Either TypeError ()
checkComparisonTypes t1 t2
  | eqTypeNormalized t1 t2 = Right ()
  | bothNumeric (stripWrap t1) (stripWrap t2) = Right ()
  | otherwise = Left $ InvalidComparison (show t1) (show t2)

stripWrap :: Type -> Type
stripWrap t = case unwrapType t of
  TKonst ty -> stripWrap ty
  TStrong ty -> stripWrap ty
  TKong ty -> stripWrap ty
  _ -> t

bothNumeric :: Type -> Type -> Bool
bothNumeric t1 t2 = case (unwrapType t1, unwrapType t2) of
  (TInt, TInt) -> True
  (TFloat, TFloat) -> True
  (TInt, TFloat) -> True
  (TFloat, TInt) -> True
  _ -> False

eqTypeNormalized :: Type -> Type -> Bool
eqTypeNormalized a b = normalize a == normalize b

normalize :: Type -> Type
normalize t = case unwrapType t of
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
normalizeExpr expr = case unwrapExpr expr of
  AValue val -> ((0, 0), AValue (normalizeValue val))
  AAccess acc -> ((0, 0), AAccess (normalizeAccess acc))
  ACall fexp args -> ((0, 0), ACall (normalizeExpr fexp) (map normalizeExpr args))
  ACast ty ex -> ((0, 0), ACast (normalize ty) (normalizeExpr ex))
  AAttribution var rhs -> ((0, 0), AAttribution var (normalizeExpr rhs))
  AMethodCall obj method args -> ((0, 0), AMethodCall (normalizeExpr obj) method (map normalizeExpr args))

normalizeValue :: AstValue -> AstValue
normalizeValue val = case unwrapValue val of
  ANumber n -> ((0, 0), ANumber n)
  AString s -> ((0, 0), AString s)
  ATuple exprs -> ((0, 0), ATuple (map normalizeExpr exprs))
  AArray exprs -> ((0, 0), AArray (map normalizeExpr exprs))
  AVector exprs -> ((0, 0), AVector (map normalizeExpr exprs))
  AStruct flds -> ((0, 0), AStruct (map (\(n, e) -> (n, normalizeExpr e)) flds))
  ALambda params ret body -> ((0, 0), ALambda params ret body)
  AVarCall v -> ((0, 0), AVarCall v)

normalizeAccess :: AstAccess -> AstAccess
normalizeAccess acc = case unwrapAccess acc of
  AArrayAccess arr idx -> ((0, 0), AArrayAccess (normalizeExpr arr) (normalizeExpr idx))
  AVectorAccess vec idx -> ((0, 0), AVectorAccess (normalizeExpr vec) (normalizeExpr idx))
  ATupleAccess tup idx -> ((0, 0), ATupleAccess (normalizeExpr tup) (normalizeExpr idx))
  AStructAccess str flds -> ((0, 0), AStructAccess (normalizeExpr str) flds)

checkFunctionCallTypes :: LineCount -> [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes lc (t:ts) (Just a:as)
  | typesEqual t a || numericCompatible t a = checkFunctionCallTypes lc ts as
  | isRefType t && typesEqual (extractRefType t) a = checkFunctionCallTypes lc ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a) lc
checkFunctionCallTypes _ [] [] = Right ()
checkFunctionCallTypes lc _ _ = Left $ InvalidArguments "Function argument count or type mismatch" lc

isRefType :: Type -> Bool
isRefType t = case unwrapType t of
  TRef _ -> True
  TKonst ty -> isRefType ty
  TStrong ty -> isRefType ty
  TKong ty -> isRefType ty
  _ -> False

extractRefType :: Type -> Type
extractRefType t = case unwrapType t of
  TRef ty -> ty
  TKonst ty -> extractRefType ty
  TStrong ty -> extractRefType ty
  TKong ty -> extractRefType ty
  _ -> t

getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrapType t of
      TKonst innerT -> case unwrapType innerT of
        TTuple ts -> if null ts then Just [] else Just (init ts)
        TFunc args _ -> Just args
        _ -> Nothing
      TFunc args _ -> Just args
      _ -> Nothing
    _ -> Nothing

getFunctionReturnType :: M.Map String Type -> String -> Maybe Type
getFunctionReturnType envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrapType t of
      TKonst innerT -> case unwrapType innerT of
        TTuple ts -> if null ts then Nothing else Just (last ts)
        TFunc _ ret -> Just ret
        _ -> Nothing
      TFunc _ ret -> Just ret
      _ -> Nothing
    _ -> Nothing

checkAssignmentType :: LineCount -> Maybe Type -> Maybe Type -> Either CompilerError ()
checkAssignmentType lc (Just expected) (Just actual)
  | eqTypeNormalized expected actual = Right ()
  | isRefType expected && eqTypeNormalized (extractRefType expected) actual = Right ()
  | otherwise = Left $ IllegalAssignment ("Type mismatch on assignment: expected " ++ show expected ++ ", got " ++ show actual) lc
checkAssignmentType lc _ _ = Left $ IllegalAssignment "Unable to infer types for assignment" lc

typesEqual :: Type -> Type -> Bool
typesEqual = eqTypeNormalized

numericCompatible :: Type -> Type -> Bool
numericCompatible a b =
  case (unwrapType (stripWrap a), unwrapType (stripWrap b)) of
    (TInt, TInt) -> True
    (TFloat, TFloat) -> True
    (TInt, TFloat) -> True
    (TFloat, TInt) -> True
    _ -> False

isFloatType :: Type -> Bool
isFloatType t = case unwrapType (stripWrap t) of
  TFloat -> True
  _ -> False

isKonst :: Type -> Bool
isKonst t = case unwrapType t of
  TKonst _ -> True
  _ -> False

inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType expr env = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    ANumber (AInteger _) -> Just (getExprLineCount expr, TInt)
    ANumber (AFloat _) -> Just (getExprLineCount expr, TFloat)
    ANumber (ABool _) -> Just (getExprLineCount expr, TBool)
    ANumber (AChar _) -> Just (getExprLineCount expr, TChar)
    AString _ -> Just (getExprLineCount expr, TString)
    ALambda _ retType _ -> Just retType
    ATuple exprs ->
      case traverse (\e -> inferType e env) exprs of
        Just ts -> Just (getExprLineCount expr, TTuple ts)
        Nothing -> Nothing
    AArray exprs -> inferHomogeneousList (getExprLineCount expr) TArray exprs env
    AVector exprs -> inferHomogeneousList (getExprLineCount expr) TVector exprs env
    AStruct _ -> Nothing
    AVarCall v ->
      case M.lookup v (typeAliases env) of
        Just t -> case unwrapType t of
          TRef ty -> Just ty
          _ -> Just t
        Nothing -> Nothing
  AAttribution _ _ -> Nothing
  AAccess acc -> inferAccessType acc env
  ACast targetType _ -> Just (resolveType env targetType)
  ACall fexp [l, r] | maybeFuncName fexp `elem` map Just arithOps ->
    case (inferType l env, inferType r env) of
      (Just t1, Just t2)
        | numericCompatible t1 t2 && (isFloatType t1 || isFloatType t2) -> Just (getExprLineCount expr, TFloat)
        | numericCompatible t1 t2 -> Just (getExprLineCount expr, TInt)
      _ -> Nothing
  ACall fexp [_l, _r] | maybeFuncName fexp `elem` map Just comparisonOps -> Just (getExprLineCount expr, TBool)
  ACall fexp _
    | maybeFuncName fexp `elem` map Just (comparisonOps ++ ["print"]) -> Nothing
    | Just name <- maybeFuncName fexp -> getFunctionReturnType (typeAliases env) name
    | otherwise -> Nothing
  AMethodCall _ _ _ -> Nothing

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/", "%"]

inferAccessType :: AstAccess -> CompilerEnv -> Maybe Type
inferAccessType acc env = case unwrapAccess acc of
  AArrayAccess arrExpr _ ->
    case inferType arrExpr env of
      Just t -> case unwrapType (stripWrap (resolveType env t)) of
        TArray et _ -> Just et
        TVector et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  AVectorAccess vecExpr _ ->
    case inferType vecExpr env of
      Just t -> case unwrapType (stripWrap (resolveType env t)) of
        TVector et _ -> Just et
        TArray et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  ATupleAccess tupleExpr idxExpr ->
    case inferType tupleExpr env of
      Just t -> case unwrapType (stripWrap (resolveType env t)) of
        TTuple ts -> tupleIndexType' ts idxExpr
        _ -> Nothing
      Nothing -> Nothing
  AStructAccess structExpr flds ->
    case inferType structExpr env of
      Just t0 -> go t0 flds
      Nothing -> Nothing
    where
      go t [] = Just (resolveType env t)
      go t (f:fs) =
        case unwrapType (stripWrap (resolveType env t)) of
          TStruct sname ->
            case M.lookup sname (structDefs env) of
              Just fds ->
                case lookup f (map (\(ty, nm) -> (nm, ty)) fds) of
                  Just fty -> go fty fs
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Nothing

tupleIndexType' :: [Type] -> AExpression -> Maybe Type
tupleIndexType' ts expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    ANumber (AInteger i)
      | i >= 0 && i < length ts -> Just (ts !! i)
      | otherwise -> Nothing
    _ -> Nothing
  _ -> Nothing

inferHomogeneousList :: LineCount -> (Type -> AExpression -> TypeRaw) -> [AExpression] -> CompilerEnv -> Maybe Type
inferHomogeneousList lc ctor exprs env =
  case traverse (\e -> inferType e env) exprs of
    Just [] -> Just (lc, ctor (lc, TInt) ((lc, AValue (lc, ANumber (AInteger 0)))))
    Just (t:ts) | all (typesEqual t) ts -> Just (lc, ctor t ((lc, AValue (lc, ANumber (AInteger (length exprs))))))
    _ -> Nothing

unwrapAst :: Ast -> AstRaw
unwrapAst (_, raw) = raw

unwrapExpr :: AExpression -> AExpressionRaw
unwrapExpr (_, raw) = raw

unwrapType :: Type -> TypeRaw
unwrapType (_, raw) = raw

unwrapAccess :: AstAccess -> AstAccessRaw
unwrapAccess (_, raw) = raw

unwrapValue :: AstValue -> AstValueRaw
unwrapValue (_, raw) = raw

getAstLineCount :: Ast -> LineCount
getAstLineCount (lc, _) = lc

getExprLineCount :: AExpression -> LineCount
getExprLineCount (lc, _) = lc

getTypeLineCount :: Type -> LineCount
getTypeLineCount (lc, _) = lc

getAccessLineCount :: AstAccess -> LineCount
getAccessLineCount (lc, _) = lc

getValueLineCount :: AstValue -> LineCount
getValueLineCount (lc, _) = lc
