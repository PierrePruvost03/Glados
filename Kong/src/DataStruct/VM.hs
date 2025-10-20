{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.VM
  ( VMState(..)
  , Env
  , Heap
  , Stack
  , HeapAddr
  , initVMState
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import DataStruct.Bytecode.Value (Value, Instr)

-- Types de base de la VM
type Env = M.Map String Value
type HeapAddr = Int
type Heap = V.Vector Value
type Stack = [Value]

-- État complet de la VM Kong
data VMState = VMState
  { stack :: Stack
  , env :: Env
  , heap :: Heap
  , code :: [Instr]
  , ip :: Int  -- Instruction Pointer
  } deriving (Show)

-- Initialisation d'un état VM vide
initVMState :: [Instr] -> VMState
initVMState instructions = VMState
  { stack = []
  , env = M.empty
  , heap = V.empty
  , code = instructions
  , ip = 0
  }
