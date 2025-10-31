module Compiler.Type.Error
  ( CompilerError(..)
  , ProgramError(..)
  ) where

import DataStruct.Ast (Ast)
import Parser (LineCount)

data CompilerError
  = UnsupportedAst String LineCount
  | IllegalAssignment String LineCount
  | UnknownVariable String LineCount
  | UnknownFunction String LineCount
  | InvalidArguments String LineCount
  | MissingMainFunction String
  | InvalidReference String LineCount
  | ReferenceToTemporary String LineCount
  | MissingReturn String LineCount
  | InvalidReturnType String LineCount
  | InvalidMainSignature String LineCount
  | UnknownStructField String LineCount
  | IndexOutOfBounds String LineCount
  | UndefinedStruct String LineCount
  | DivisionByZero String LineCount
  | ConstantOverflow String LineCount
  | InvalidCast String LineCount
  | KonstModification String LineCount
  | NonCallableType String LineCount
  | IncompatibleOperands String LineCount
  | DuplicateDeclaration String LineCount
  | UninitializedVariable String LineCount
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)
