module Compiler.Types
  ( CompilerError(..)
  , ProgramError(..)
  , CompilerEnv
  ) where

import DataStruct.Ast (Ast, Type)
import qualified Data.Map as M

type CompilerEnv = M.Map String Type

data CompilerError
  = UnsupportedAst String
  | IllegalAssignment String
  | UnknownVariable String
  | UnknownFunction String
  | InvalidArguments String
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)
