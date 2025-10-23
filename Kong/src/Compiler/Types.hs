module Compiler.Types
  ( CompilerError(..)
  , ProgramError(..)
  ) where

import DataStruct.Ast (Ast)

data CompilerError
  = UnsupportedAst String
  | UnknownVariable String
  | UnknownFunction String
  | InvalidArguments String
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)
