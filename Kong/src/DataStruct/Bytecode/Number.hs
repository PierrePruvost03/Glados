{-# LANGUAGE LambdaCase #-}
module DataStruct.Bytecode.Number where

import DataStruct.Bytecode.Utils (construct)
import Data.Binary

data Number
  = VInt Int
  | VBool Bool
  | VChar Char
  | VFloat Double
  deriving (Eq, Show)

instance Binary Number where
    -- writing
    put (VInt v) = put (0 :: Word8) <> put v
    put (VFloat v) = put (1 :: Word8) <> put v
    put (VChar v) = put (2 :: Word8) <> put v
    put (VBool v) = put (3 :: Word8) <> put v
    -- reading
    get = (get :: Get Word8) >>= \case
        0 -> construct VInt
        1 -> construct VFloat
        2 -> construct VChar
        3 -> construct VBool
        _ -> fail "Unknow number"
