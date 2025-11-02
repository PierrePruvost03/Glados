module VM.EnvGestion (mergeEnv) where

import DataStruct.VM (ExecEnv)
import Data.Maybe (catMaybes)
import qualified Data.Map as M

mergeEnv :: ExecEnv -> [String] -> ExecEnv
mergeEnv current called = M.fromList $ catMaybes $ f called
    where
        f [] = []
        f (n:xs) = (current M.!? n >>= \v -> Just (n ,v)) : f xs
