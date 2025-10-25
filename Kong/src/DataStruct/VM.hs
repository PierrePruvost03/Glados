{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.VM
  ( VMState(..)
  , ExecEnv
  , Heap
  , Stack
  , HeapAddr
  , initVMState
  , baseState
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import DataStruct.Bytecode.Value (Value, Instr)

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
  } deriving (Show)

baseState :: [Instr] -> VMState
baseState instr = VMState {stack = [], env = M.empty, heap = V.empty, code = V.fromList instr, ip = 0}

-- Initialisation d'un état VM vide
initVMState :: [Instr] -> VMState
initVMState instructions = VMState
  { stack = []
  , env = M.empty
  , heap = V.empty
  , code = V.fromList instructions
  , ip = 0
  }
