{-# LANGUAGE LambdaCase #-}
module DataStruct.Bytecode.Syscall (Syscall(..), put, get) where

import Data.Binary
import DataStruct.Bytecode.Utils (construct)

data Syscall
    = Exit
    | Print Int
    | Read
    | Write
    | Open
    | Close
    deriving (Show, Eq)

instance Binary Syscall where
    -- writing
    put Exit = put (0 :: Word8)
    put (Print n) = put (1 :: Word8) <> put n
    put Read = put (2 :: Word8)
    put Write = put (3 :: Word8)
    put Open = put (4 :: Word8)
    put Close = put (5 :: Word8)
    -- reading
    get = (get :: Get Word8) >>= \case
        0 -> return Exit
        1 -> construct Print
        2 -> return Read
        3 -> return Write
        4 -> return Open
        5 -> return Close
        _ -> fail "Unknow syscall"
