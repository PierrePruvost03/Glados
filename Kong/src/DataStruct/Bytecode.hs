{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE LambdaCase #-}

module DataStruct.Bytecode
  ( Value(..)
  , Instr(..)
  , Op(..)
  , builtinOps
  , stringToOp
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Env = M.Map String Value
type HeapAddr = Int

data Value
  = VInt Int
  | VBool Bool
  | VChar Char
  | VString String
  | VFloat Double
  | VTuple (V.Vector Value)
  | VArray (V.Vector Value) Bool        -- Bool = isKonst
  | VVector (V.Vector Value)
  | VList (V.Vector Value)
  | VStruct String (M.Map String HeapAddr)
  | VFunction [String] [Instr] Env
  | VBuiltinOp Op
  | VRef HeapAddr
  | VEmpty
  deriving (Eq, Show)

data Op
  = Add | Sub | Mul | Div 
  | Equal | Lt | Gt | Le | Ge | Ne
  | And | Or | Not
  deriving (Eq, Ord, Show)

builtinOps :: [String]
builtinOps = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "!="]

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
  op -> error $ "Unknown operator: " ++ op

data Instr
  = Push Value
  | PushEnv String
  | Call
  | Ret
  | Nop

  -- Assignations
  | SetVar String
  | SetArray Int
  | SetVector Int
  | SetStruct String
  | SetTuple Int

  -- Accès
  | GetArray Int
  | ArrayGet         -- Pour l'accès aux tableaux génériques
  | GetVector Int
  | GetStruct String
  | GetTuple Int

  -- Sauts
  | Jump Int
  | JumpIfFalse Int
  | JumpIfTrue Int

  -- Opérations natives
  | DoOp Op

  -- Fonctions
  | PushLambda [String] [Instr]

  -- Heap
  | Alloc
  | LoadRef
  | StoreRef
  deriving (Eq, Show)
