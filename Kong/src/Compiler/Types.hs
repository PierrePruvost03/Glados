module Compiler.Types
  ( CompilerError(..)
  , ProgramError(..)
  , CompilerEnv (..)
  , emptyEnv
  , insertTypeAlias
  , resolveType
  , checkComparisonTypes
  , eqTypeNormalized
  ) where

import DataStruct.Ast (Ast (..), Type (..))
import qualified Data.Map as M
import Compiler.TypeError (TypeError(..))

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
insertTypeAlias env (AStruktDef name fields) = env { structDefs = M.insert name fields (structDefs env) }
insertTypeAlias env _ = env

resolveType :: CompilerEnv -> Type -> Type
resolveType env t@(TCustom name) =
  case M.lookup name (typeAliases env) of
    Just realTy -> resolveType env realTy
    Nothing -> t
resolveType env (TKonst ty) = TKonst (resolveType env ty)
resolveType env (TStrong ty) = TStrong (resolveType env ty)
resolveType env (TKong ty) = TKong (resolveType env ty)
resolveType _ (TStruct s) = TStruct s
resolveType _ (TTrait s) = TTrait s
resolveType env (TArray ty e) = TArray (resolveType env ty) e
resolveType env (TVector ty e) = TVector (resolveType env ty) e
resolveType env (TTuple tys) = TTuple (map (resolveType env) tys)
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

-- Structural type equality ignoring wrappers (TKonst, TStrong, TKong)
eqTypeNormalized :: Type -> Type -> Bool
eqTypeNormalized a b = eqType (stripWrap a) (stripWrap b)

eqType :: Type -> Type -> Bool
eqType TInt TInt = True
eqType TBool TBool = True
eqType TChar TChar = True
eqType TString TString = True
eqType TFloat TFloat = True
eqType (TStruct s1) (TStruct s2) = s1 == s2
eqType (TTrait s1) (TTrait s2) = s1 == s2
eqType (TCustom s1) (TCustom s2) = s1 == s2
eqType (TArray t1 _) (TArray t2 _) = eqType t1 t2
eqType (TVector t1 _) (TVector t2 _) = eqType t1 t2
eqType (TTuple xs) (TTuple ys) = length xs == length ys && and (zipWith eqType xs ys)
eqType _ _ = False
