{-# LANGUAGE LambdaCase #-}
module DataStruct.Bytecode.Number (Number(..), NumberType(..), put, get) where

import DataStruct.Bytecode.Utils (construct)
import Data.Binary
import Data.Word (Word32)
import Data.Int (Int32, Int64)

data Number
  = VInt Int32
  | VBool Bool
  | VChar Char
  | VFloat Double
  | VLong Int64
  | VUInt Word32
  deriving (Eq, Ord)

instance Show Number where
    show (VInt i) = show i
    show (VBool b) = show b
    show (VChar c) = show c
    show (VFloat f) = show f
    show (VLong l) = show l
    show (VUInt u) = show u

instance Binary Number where
    -- writing
    put (VInt v) = put (0 :: Word8) <> put v
    put (VFloat v) = put (1 :: Word8) <> put v
    put (VChar v) = put (2 :: Word8) <> put v
    put (VBool v) = put (3 :: Word8) <> put v
    put (VLong v) = put (4 :: Word8) <> put v
    put (VUInt v) = put (5 :: Word8) <> put v
    -- reading
    get = (get :: Get Word8) >>= \case
        0 -> construct VInt
        1 -> construct VFloat
        2 -> construct VChar
        3 -> construct VBool
        4 -> construct VLong
        5 -> construct VUInt
        _ -> fail "Unknow number"

data NumberType
    = NTInt
    | NTBool
    | NTChar
    | NTFloat
    | NTLong
    | NTUInt
    deriving (Eq, Show)

instance Binary NumberType where
    -- writing
    put NTInt = put (0 :: Word8)
    put NTFloat = put (1 :: Word8)
    put NTChar = put (2 :: Word8)
    put NTBool = put (3 :: Word8)
    put NTLong = put (4 :: Word8)
    put NTUInt = put (5 :: Word8)
    -- reading
    get = (get :: Get Word8) >>= \case
        0 -> return NTInt
        1 -> return NTFloat
        2 -> return NTChar
        3 -> return NTBool
        4 -> return NTLong
        5 -> return NTUInt
        _ -> fail "Unknow number type"
