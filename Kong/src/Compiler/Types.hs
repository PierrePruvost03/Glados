module Compiler.Types
  ( CompilerError(..)
  , ProgramError(..)
  , CompilerEnv (..)
  , emptyEnv
  , insertTypeAlias
  , resolveType
  ) where

import DataStruct.Ast (Ast (..), Type (..))
import qualified Data.Map as M

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
