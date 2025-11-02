module Compiler.Type.Inference
  ( inferType
  , inferAccessType
  , inferHomogeneousList
  , getTupleIndexType
  , getFunctionReturnType
  , getFunctionArgTypes
  , resolveType
  , CompilerEnv(..)
  , emptyEnv
  , insertInEnv
  ) where

import DataStruct.Ast
import Parser (LineCount)
import qualified Data.Map as M
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.Type.Normalization (stripWrap, eqTypeNormalized, typeToString, eqTypeNormalized)
import Compiler.Type.Checks (isFloatType, numericCompatible, comparisonOps, arithOps, logicalOps)

data CompilerEnv = CompilerEnv
  { typeAliases :: M.Map String Type
  , structDefs  :: M.Map String [(Type, String)]
  , traitDefs   :: M.Map String [(String, [Type], Type)]  -- trait name -> methods (name, args, ret)
  , traitImpls  :: M.Map (String, String) [String]  -- (trait, type) -> method names
  } deriving (Show, Eq)

emptyEnv :: CompilerEnv
emptyEnv = initBuiltinFunctions (CompilerEnv M.empty M.empty M.empty M.empty)

-- Initialize environment with builtin functions (push, pop, len, ++)
initBuiltinFunctions :: CompilerEnv -> CompilerEnv
initBuiltinFunctions env = env { typeAliases = M.union builtins (typeAliases env) }
  where
    lc0 = (0, 0)
    genericT = (lc0, TCustom "T")
    genericVec = (lc0, TVector genericT (lc0, AValue (lc0, ANumber (AInteger 0))))
    tInt = (lc0, TInt)
    tChar = (lc0, TChar)
    tVoid = (lc0, TCustom "Void")
    vecChar0 = (lc0, TVector tChar (lc0, AValue (lc0, ANumber (AInteger 0))))
    vecVecChar0 = (lc0, TVector vecChar0 (lc0, AValue (lc0, ANumber (AInteger 0))))

    builtins = M.fromList
      [ ("$push", (lc0, TKonst (lc0, TFunc [genericVec, genericT] (lc0, TInt))))
      , ("$pop", (lc0, TKonst (lc0, TFunc [genericVec] genericT)))
      , ("$len", (lc0, TKonst (lc0, TFunc [genericVec] (lc0, TInt))))
      , ("++", (lc0, TKonst (lc0, TFunc [(lc0, TRef (lc0, TInt))] (lc0, TInt))))
      -- Typed signatures for syscalls (codegen still emits Syscall instructions)
      , ("open",   (lc0, TKonst (lc0, TFunc [vecChar0] tInt)))
      , ("read",   (lc0, TKonst (lc0, TFunc [tInt, tInt] vecChar0)))
      , ("write",  (lc0, TKonst (lc0, TFunc [tInt, vecChar0] tInt)))
      , ("close",  (lc0, TKonst (lc0, TFunc [tInt] tVoid)))
      , ("exit",   (lc0, TKonst (lc0, TFunc [tInt] tVoid)))
      , ("getArgv",(lc0, TKonst (lc0, TFunc [] vecVecChar0)))
      ]

-- Insert a type alias or struct definition into the environment
insertInEnv :: CompilerEnv -> Ast -> CompilerEnv
insertInEnv env ast = case unwrap ast of
  ATypeAlias name ty -> env { typeAliases = M.insert name ty (typeAliases env) }
  AStruktDef name fds -> env { structDefs = M.insert name fds (structDefs env) }
  ATraitDef name methods -> env { traitDefs = M.insert name methods (traitDefs env) }
  ATraitImpl tName implType _ ->
    env { traitImpls = M.insert
            (tName, typeToString implType)
            (maybe [] (map (\(n, _, _) -> n)) (M.lookup tName (traitDefs env)))
            (traitImpls env) }
  _ -> env

-- Resolve a type through the environment
resolveType :: CompilerEnv -> Type -> Type
resolveType env t = case unwrap t of
  TString -> (lc t, TVector (lc t, TChar) (lc t, AValue (lc t, ANumber (AInteger 0))))
  TCustom name ->
    case M.lookup name (typeAliases env) of
      Just realTy -> resolveType env realTy
      Nothing -> case M.lookup name (structDefs env) of
        Just _ -> (lc t, TStruct name)
        Nothing -> t
  TKonst ty -> (lc t, TKonst (resolveType env ty))
  TStrong ty -> (lc t, TStrong (resolveType env ty))
  TKong ty -> (lc t, TKong (resolveType env ty))
  TRef ty -> (lc t, TRef (resolveType env ty))
  TStruct s -> (lc t, TStruct s)
  TTrait s -> (lc t, TTrait s)
  TArray ty e -> (lc t, TArray (resolveType env ty) e)
  TVector ty e -> (lc t, TVector (resolveType env ty) e)
  TTuple tys -> (lc t, TTuple (map (resolveType env) tys))
  TFunc args ret -> (lc t, TFunc (map (resolveType env) args) (resolveType env ret))
  raw -> (lc t, raw)

-- extract function name from expression
maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall n -> Just n
    _ -> Nothing
  _ -> Nothing

-- Extract the type from a parameter (AVarDecl)
extractParamType :: Ast -> Maybe Type
extractParamType param = case unwrap param of
  AVarDecl t _ _ -> Just t
  _ -> Nothing

-- Infer the type of an expression
inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType expr env = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger _) -> Just (lc expr, TInt)
    ANumber (AFloat _) -> Just (lc expr, TFloat)
    ANumber (ABool _) -> Just (lc expr, TBool)
    ANumber (AChar _) -> Just (lc expr, TChar)
    AString _ ->
      Just (lc expr, TVector (lc expr, TChar) (lc expr, AValue (lc expr, ANumber (AInteger 0))))
    ALambda params retType _ ->
      case traverse extractParamType params of
        Just paramTypes -> Just (lc expr, TFunc paramTypes retType)
        Nothing -> Nothing
    ATuple exprs ->
      case traverse (\e -> inferType e env) exprs of
        Just ts -> Just (lc expr, TTuple ts)
        Nothing -> Nothing
    AArray exprs -> inferHomogeneousList (lc expr) TArray exprs env
    AVector exprs -> inferHomogeneousList (lc expr) TVector exprs env
    AStruct _ -> Nothing
    AVarCall v ->
      case M.lookup v (typeAliases env) of
        Just t -> case unwrap t of
          TRef ty -> Just ty
          _ -> Just t
        Nothing -> Nothing
  AAttribution _ _ -> Nothing
  AAccess acc -> inferAccessType acc env
  ACast targetType _ -> Just (resolveType env targetType)
  ACall fexp [l, r] | maybeFuncName fexp `elem` map Just arithOps ->
    case (inferType l env, inferType r env) of
      (Just t1, Just t2)
        | numericCompatible t1 t2 && (isFloatType t1 || isFloatType t2) -> Just (lc expr, TFloat)
        | numericCompatible t1 t2 -> Just (lc expr, TInt)
      _ -> Nothing
  ACall fexp [_l, _r] | maybeFuncName fexp `elem` map Just comparisonOps -> Just (lc expr, TBool)
  ACall fexp [_l, _r] | maybeFuncName fexp `elem` map Just logicalOps -> Just (lc expr, TBool)
  ACall fexp _
    | maybeFuncName fexp `elem` map Just (comparisonOps ++ ["print"]) -> Nothing
    | Just name <- maybeFuncName fexp -> getFunctionReturnType (typeAliases env) name
    | otherwise -> Nothing
  AMethodCall expression name _ -> inferType expression env >>= \t ->
    getFunctionReturnType (typeAliases env) (typeToString (stripWrap t) <> ('$':name))

-- Infer type from an access expression
inferAccessType :: AstAccess -> CompilerEnv -> Maybe Type
inferAccessType acc env = case unwrap acc of
  AArrayAccess arrExpr _ ->
    case inferType arrExpr env of
      Just t -> case unwrap (stripWrap (resolveType env t)) of
        TArray et _ -> Just et
        TVector et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  AVectorAccess vecExpr _ ->
    case inferType vecExpr env of
      Just t -> case unwrap (stripWrap (resolveType env t)) of
        TVector et _ -> Just et
        TArray et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  ATupleAccess tupleExpr idxExpr ->
    case inferType tupleExpr env of
      Just t -> case unwrap (stripWrap (resolveType env t)) of
        TTuple ts -> getTupleIndexType ts idxExpr
        _ -> Nothing
      Nothing -> Nothing
  AStructAccess structExpr flds ->
    case inferType structExpr env of
      Just t0 -> go t0 flds
      Nothing -> Nothing
    where
      go t [] = Just (resolveType env t)
      go t (f:fs) =
        case unwrap (stripWrap (resolveType env t)) of
          TStruct sname ->
            case M.lookup sname (structDefs env) of
              Just fds ->
                case lookup f (map (\(ty, nm) -> (nm, ty)) fds) of
                  Just fty -> go fty fs
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Nothing

-- Get type from tuple index
getTupleIndexType :: [Type] -> AExpression -> Maybe Type
getTupleIndexType ts expr = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger i)
      | i >= 0 && i < length ts -> Just (ts !! i)
      | otherwise -> Nothing
    _ -> Nothing
  _ -> Nothing

-- Infer type of homogeneous list (Array or Vector)
inferHomogeneousList :: LineCount -> (Type -> AExpression -> TypeRaw) -> [AExpression] -> CompilerEnv -> Maybe Type
inferHomogeneousList lnCount ctor exprs env =
  case traverse (\e -> inferType e env) exprs of
    Just [] -> Just (lnCount, ctor (lnCount, TInt) ((lnCount, AValue (lnCount, ANumber (AInteger 0)))))
    Just (t:ts) | all (eqTypeNormalized t) ts -> Just (lnCount, ctor t ((lnCount, AValue (lnCount, ANumber (AInteger (length exprs))))))
    _ -> Nothing

-- Get function argument types from environment
getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrap t of
      TKonst innerT -> case unwrap innerT of
        TTuple ts -> case ts of
          [] -> Just []
          _ -> Just (init ts)
        TFunc args _ -> Just args
        _ -> Nothing
      TFunc args _ -> Just args
      _ -> Nothing
    _ -> Nothing

-- Get function return type from environment
getFunctionReturnType :: M.Map String Type -> String -> Maybe Type
getFunctionReturnType envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrap t of
      TKonst innerT -> case unwrap innerT of
        TTuple ts -> case ts of
          [] -> Nothing
          _ -> Just (last ts)
        TFunc _ ret -> Just ret
        _ -> Nothing
      TFunc _ ret -> Just ret
      _ -> Nothing
    _ -> Nothing
