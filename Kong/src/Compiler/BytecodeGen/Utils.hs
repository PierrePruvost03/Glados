module Compiler.BytecodeGen.Utils
  ( extractParamNames
  , extractGlobalNames
  , prebindVar
  , prebindKonsts
  , extractArraySize
  ) where

import DataStruct.Ast
import Compiler.Type.Inference (CompilerEnv(..))
import Compiler.Unwrap (Unwrappable(..))
import qualified Data.Map as M

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam :: Ast -> [String] -> [String]
    extractParam ast acc = case unwrap ast of
      AVarDecl _ name _ -> name : acc
      _ -> acc

-- Extract all keys from a Map
extractGlobalNames :: M.Map String a -> [String]
extractGlobalNames = M.keys

-- Prebind a variable in the environment if it's a const type
-- This is necessary for const variables to be visible in their own initializer scope
prebindVar :: Type -> String -> CompilerEnv -> CompilerEnv
prebindVar t name env = case unwrap t of
  TKonst _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

-- Prebind all const variables in a block to make them visible in the block scope
-- This handles forward references for const declarations
prebindKonsts :: [Ast] -> CompilerEnv -> CompilerEnv
prebindKonsts asts env = foldl step env asts
  where
    step :: CompilerEnv -> Ast -> CompilerEnv
    step e ast = case unwrap ast of
      AVarDecl t n _ | isKonstType t -> e { typeAliases = M.insert n t (typeAliases e) }
      _ -> e
    isKonstType :: Type -> Bool
    isKonstType t = case unwrap t of
      TKonst _ -> True
      _ -> False

-- Extract the size from an array size expression constant
extractArraySize :: AExpression -> Maybe Int
extractArraySize expr = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger n) -> Just n
    _ -> Nothing
  _ -> Nothing
