module Interpreter.BaseEnv
  ( emptyEnv
  , extendEnv
  , lookupEnv
  , err
  , defaultEnv
  ) where

import qualified Data.Map.Strict as Map
import Parser (LineCount)
import DataStruct.Value (Value(..), Env)
import Interpreter.Primitives.Primitives (primitiveList)

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Env -> [(String, Value)] -> Env
extendEnv = foldr (uncurry Map.insert)

lookupEnv :: String -> Env -> Maybe Value
lookupEnv = Map.lookup

defaultEnv :: Env
defaultEnv = extendEnv emptyEnv primitiveList

err :: LineCount -> String -> Either String a
err (l,c) msg = Left $ "Error at " ++ show (l,c) ++ ": " ++ msg
