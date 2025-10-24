{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteVM (executeVM) where

import VM.Execution (exec)
import VM.Operations (makeIntValue)
import DataStruct.Bytecode.Value (Instr)
import Data.Binary (decodeFile)
import DataStruct.VM (VMState(..), baseState)
import VM.Errors (ExecError(..))
import Control.Exception
import System.IO

executeVM :: String -> IO Int
executeVM file = (((decodeFile file) :: IO [Instr]) >>= \instr -> exec (baseState instr) >>= \case
        (VMState {stack = x : _}) -> pure (makeIntValue x)
        _ -> throwIO $ InvalidStackAccess
    ) `catches`  [
        Handler (\(ex :: ExecError) -> hPutStrLn stderr (show ex) >> pure 1),
        Handler (\(_ :: ErrorCall) -> hPutStrLn stderr
            "Failed to decode binary file" >> pure 1),
        Handler (\(_ :: IOException) -> hPutStrLn stderr (
            "Binary file " <> file <> " does not exist or cannot be read: ") >> pure 1)
    ]
