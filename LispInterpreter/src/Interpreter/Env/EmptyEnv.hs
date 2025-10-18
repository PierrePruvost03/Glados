module Interpreter.Env.EmptyEnv (emptyEnv) where

import qualified Data.Map.Strict as Map
import DataStruct.Value

emptyEnv :: Env
emptyEnv = Map.empty
