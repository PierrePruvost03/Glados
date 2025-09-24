{-# OPTIONS_GHC -Wno-partial-fields #-}

module Interpreter.BaseEnv
  ( Value(..)
  , Env
  , emptyEnv
  , extendEnv
  , lookupEnv
  , err
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parser (LineCount)
import DataStruct.Ast (Ast)

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VList [Value]
  | VLambda { vLParams :: [String], vLBody :: Ast, vLEnv :: Env }
  | VPrim { primName :: String, primImpl :: [Value] -> Either String Value }

type Env = Map String Value

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Env -> [(String, Value)] -> Env
extendEnv = foldr (uncurry Map.insert)

lookupEnv :: String -> Env -> Maybe Value
lookupEnv = Map.lookup

err :: LineCount -> String -> Either String a
err (l,c) msg = Left $ "Error at " ++ show (l,c) ++ ": " ++ msg
