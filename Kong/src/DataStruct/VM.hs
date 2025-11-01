{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.VM
  ( VMState(..)
  , ExecEnv
  , Heap
  , Stack
  , HeapAddr
  , baseState
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import DataStruct.Bytecode.Value (Value(..), Instr(..))
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (Op(..))

-- Types de base de la VM
type ExecEnv = M.Map String Value
type HeapAddr = Int
type Heap = V.Vector Value
type Stack = [Value]
type Code = V.Vector Instr

-- État complet de la VM Kong
data VMState = VMState
  { stack :: Stack
  , env :: ExecEnv
  , heap :: Heap
  , code :: Code
  , ip :: Int  -- Instruction Pointer
  } deriving (Show, Eq)

baseState :: [Instr] -> VMState
baseState instr = VMState {stack = [], env = M.fromList ([
        ("$push", VFunction [] (V.fromList [
                SetVar "list",  -- (val, xs)
                PushEnv "list", -- (ref, val, xs)
                LoadRef,        -- (list, val, xs)
                ListPush,       -- (new, xs)
                PushEnv "list", -- (ref, new, xs)
                StoreRef,       -- (xs)
                PushEnv "list", -- (ref, xs)
                LoadRef,        -- (new, xs)
                Ret
            ])
        ),
        ("$pop", VFunction [] (V.fromList [
                SetVar "list",  -- (xs)
                Push (VNumber (VInt 1)), -- (1, xs)
                PushEnv "list", -- (ref, 1, xs)
                LoadRef,        -- (list, 1,xs)
                Length,         -- (len, 1 xs)
                DoOp Sub,
                PushEnv "list", -- (ref, len, xs)
                LoadRef,        -- (list, len, xs)
                GetList,        -- (last, xs)
                SetVar "last",
                PushEnv "list", -- (ref, xs)
                LoadRef,        -- (list, xs)
                ListPop,        -- (newList, xs)
                PushEnv "list", -- (ref, newList, last, xs)
                StoreRef,       -- (last, xs)
                PushEnv "last",
                LoadRef,
                Ret
            ])
        ),
        ("$len", VFunction [] (V.fromList [
                LoadRef,
                Length,
                Ret
            ])
        )
    ]), heap = V.empty, code = V.fromList instr, ip = 0}
