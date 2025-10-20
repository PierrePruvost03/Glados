module Interpreter.Env.BaseEnv
  ( extendEnv,
    lookupEnv,
    err,
    defaultEnv,
  )
where

import qualified Data.Map.Strict as Map
import DataStruct.Value (Env, Value (..))
import Interpreter.Env.EmptyEnv
import Interpreter.Primitives.Primitives (primitiveList)
import Parser (LineCount)

extendEnv :: Env -> [(String, Value)] -> Env
extendEnv = foldr (uncurry Map.insert)

lookupEnv :: String -> Env -> Maybe Value
lookupEnv = Map.lookup

defaultEnv :: Env
defaultEnv = extendEnv emptyEnv primitiveList

err :: LineCount -> String -> Either String a
err (l, c) msg = Left $ "Error at " ++ show (l, c) ++ ": " ++ msg
