{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.VM
  ( VMState(..)
  , ExecError(..)
  , ExecEnv
  , Heap
  , Stack
  , HeapAddr
  , initVMState
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import DataStruct.Bytecode.Value (Value, Instr)
import Control.Exception

-- Types de base de la VM
type ExecEnv = M.Map String Value
type HeapAddr = Int
type Heap = V.Vector Value
type Stack = [Value]
type Code = V.Vector Instr

data ExecError = Err Int

instance Show ExecError where
    show (Err 0) = "Error: Impossible instruction or wrong arguments"
    show (Err 1) = "Error: Ending code without a return value or jumping out of range"
    show (Err 2) = "Error: Impossible operation - division by zero"
    show _ = "Unknow error"

instance Exception ExecError

-- État complet de la VM Kong
data VMState = VMState
  { stack :: Stack
  , env :: ExecEnv
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
