{-# LANGUAGE LambdaCase #-}
module DataStruct.Bytecode.Op (Op(..), builtinOps, stringToOp, get, put) where

import Data.Binary

data Op
  = Add | Sub | Mul | Div
  | Equal | Lt | Gt | Le | Ge | Ne
  | And | Or | Not | Mod
  deriving (Eq, Ord, Show)

builtinOps :: [String]
builtinOps = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "!=", "%"]

stringToOp :: String -> Op
stringToOp = \case
  "+" -> Add
  "-" -> Sub
  "*" -> Mul
  "/" -> Div
  "==" -> Equal
  "<" -> Lt
  ">" -> Gt
  "<=" -> Le
  ">=" -> Ge
  "!=" -> Ne
  "%" -> Mod
  op -> error $ "Unknown operator: " ++ op

instance Binary Op where
    -- write
    put Add = put (0 :: Word8)
    put Sub = put (1 :: Word8)
    put Mul = put (2 :: Word8)
    put Div = put (3 :: Word8)
    put Equal = put (4 :: Word8)
    put Lt = put (5 :: Word8)
    put Gt = put (6 :: Word8)
    put Le = put (7 :: Word8)
    put Ge = put (8 :: Word8)
    put Ne = put (9 :: Word8)
    put And = put (10 :: Word8)
    put Or = put (11 :: Word8)
    put Not = put (12 :: Word8)
    put Mod = put (13 :: Word8)
    -- read
    get = (get :: Get Word8) >>= \case
        0 -> return Add
        1 -> return Sub
        2 -> return Mul
        3 -> return Div
        4 -> return Equal
        5 -> return Lt
        6 -> return Gt
        7 -> return Le
        8 -> return Ge
        9 -> return Ne
        10 -> return And
        11 -> return Or
        12 -> return Not
        13 -> return Mod
        _ -> fail "Unknown operation"
