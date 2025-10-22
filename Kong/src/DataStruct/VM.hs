{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.VM
  ( VMState(..)
  , Env
  , Heap
  , Stack
  , HeapAddr
  , initVMState
  , MemoryCell
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import DataStruct.Bytecode.Value (Value, Instr)

-- Types de base de la VM
data MemoryCell = THEAP | TSTACK deriving (Show, Eq)
type Env = M.Map String (Value, MemoryCell)
type HeapAddr = Int
type Heap = V.Vector Value
type Stack = [Value]
type Code = V.Vector Instr

-- État complet de la VM Kong
data VMState = VMState
  { stack :: Stack
  , env :: Env
  , heap :: Heap
  , code :: Code
  , ip :: Int  -- Instruction Pointer
  } deriving (Show)

-- Initialisation d'un état VM vide
initVMState :: [Instr] -> VMState
initVMState instructions = VMState
  { stack = []
  , env = M.empty
  , heap = V.empty
  , code = V.fromList instructions
  , ip = 0
  }
