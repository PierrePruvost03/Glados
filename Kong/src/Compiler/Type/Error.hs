module Compiler.Type.Error
  ( CompilerError(..)
  , ProgramError(..)
  , prettyError
  ) where

import DataStruct.Ast (Ast, Type)
import Parser (LineCount)

data CompilerError
  = UnsupportedAst String LineCount
  | TypeMismatch Type Type LineCount
  | InvalidComparison Type Type LineCount
  | UnknownVariable String LineCount
  | UnknownFunction String LineCount
  | InvalidLeftHandSide LineCount
  | InvalidArrayAccess LineCount
  | InvalidTupleAccess LineCount
  | InvalidStructAccess LineCount
  | TupleIndexNotConstant LineCount
  | ArgumentCountMismatch Int Int LineCount
  | ArgumentTypeMismatch Int Type Type LineCount
  | MissingMainFunction
  | InvalidReferenceTarget String LineCount
  | ReferenceToTemporary String LineCount
  | MissingReturn LineCount
  | ReturnTypeMismatch Type Type LineCount
  | MainSignatureError String LineCount
  | UnknownStructField String String LineCount
  | IndexOutOfBounds Int Int LineCount
  | NegativeIndex Int LineCount
  | UndefinedStruct String String LineCount
  | DivisionByZero LineCount
  | ConstantOverflow Integer String LineCount
  | InvalidCast Type Type LineCount
  | KonstModification String LineCount
  | NonCallableType String Type LineCount
  | IncompatibleOperands String Type Type LineCount
  | DuplicateDeclaration String String LineCount
  | UninitializedVariable String LineCount
  | CannotInferType LineCount
  | InvalidArguments String LineCount
  | IllegalAssignment String LineCount
  | InvalidReturnType String LineCount
  | InvalidMainSignature String LineCount
  | InvalidReference String LineCount
  | UndefinedTrait String LineCount
  | MissingTraitMethod String String String LineCount
  | UnexpectedTraitMethod String String String LineCount
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)

prettyError :: CompilerError -> String
prettyError err = case err of
  UnsupportedAst msg lc -> linePrefix lc ++ "Unsupported AST: " ++ msg
  TypeMismatch expect act lc -> linePrefix lc ++ "Type mismatch: expected '" ++ show expect ++ "', got '" ++ show act ++ "'"
  InvalidComparison l r lc -> linePrefix lc ++ "Invalid comparison between '" ++ show l ++ "' and '" ++ show r ++ "' (non-comparable types)"
  UnknownVariable v lc -> linePrefix lc ++ "Unknown variable '" ++ v ++ "'"
  UnknownFunction f lc -> linePrefix lc ++ "Unknown function '" ++ f ++ "'"
  InvalidLeftHandSide lc -> linePrefix lc ++ "Invalid left-hand side for assignment"
  InvalidArrayAccess lc -> linePrefix lc ++ "Array access on non-array type"
  InvalidTupleAccess lc -> linePrefix lc ++ "Tuple access on non-tuple type"
  InvalidStructAccess lc -> linePrefix lc ++ "Struct access on non-struct type"
  TupleIndexNotConstant lc -> linePrefix lc ++ "Tuple index must be a constant integer"
  ArgumentCountMismatch expect act lc -> 
    linePrefix lc ++ "Argument count mismatch: expected " ++ show expect ++ " arguments, got " ++ show act
  ArgumentTypeMismatch pos expT actT lc -> 
    linePrefix lc ++ "Argument " ++ show pos ++ " type mismatch: expected '" ++ show expT ++ "', got '" ++ show actT ++ "'"
  MissingMainFunction -> "Missing main function"
  InvalidReferenceTarget e lc -> linePrefix lc ++ "Cannot create reference to: " ++ e
  ReferenceToTemporary e lc -> linePrefix lc ++ "Cannot create reference to temporary value: " ++ e
  MissingReturn lc -> linePrefix lc ++ "Function with non-void return type must have a return statement"
  ReturnTypeMismatch expect act lc -> linePrefix lc ++ "Return type mismatch: expected '" ++ show expect ++ "', got '" ++ show act ++ "'"
  MainSignatureError reason lc -> linePrefix lc ++ "Invalid main function signature: " ++ reason
  UnknownStructField s f lc -> linePrefix lc ++ "Field '" ++ f ++ "' does not exist in struct '" ++ s ++ "'"
  IndexOutOfBounds idx sz lc -> linePrefix lc ++ "Index " ++ show idx ++ " is out of bounds (size is " ++ show sz ++ ")"
  NegativeIndex idx lc -> linePrefix lc ++ "Index " ++ show idx ++ " is negative"
  UndefinedStruct s ctx lc -> linePrefix lc ++ "Struct '" ++ s ++ "' is not defined" ++ (if null ctx then "" else " (used in " ++ ctx ++ ")")
  DivisionByZero lc -> linePrefix lc ++ "Division by zero"
  ConstantOverflow val typ lc -> linePrefix lc ++ "Constant " ++ show val ++ " out of bounds for " ++ typ
  InvalidCast from to lc -> linePrefix lc ++ "Cannot cast from '" ++ show from ++ "' to '" ++ show to ++ "'"
  KonstModification v lc -> linePrefix lc ++ "Cannot modify Konst variable '" ++ v ++ "'"
  NonCallableType v t lc -> linePrefix lc ++ "'" ++ v ++ "' of type '" ++ show t ++ "' is not callable"
  IncompatibleOperands op l r lc -> 
    linePrefix lc ++ "Operator '" ++ op ++ "' cannot be applied to types '" ++ show l ++ "' and '" ++ show r ++ "'"
  DuplicateDeclaration n k lc -> linePrefix lc ++ k ++ " '" ++ n ++ "' is already declared"
  UninitializedVariable v lc -> linePrefix lc ++ "Variable '" ++ v ++ "' is not initialized"
  CannotInferType lc -> linePrefix lc ++ "Cannot infer type"
  InvalidArguments msg lc -> linePrefix lc ++ msg
  IllegalAssignment msg lc -> linePrefix lc ++ "Illegal assignment: " ++ msg
  InvalidReturnType msg lc -> linePrefix lc ++ "Invalid return type: " ++ msg
  InvalidMainSignature msg lc -> linePrefix lc ++ "Invalid main signature: " ++ msg
  InvalidReference msg lc -> linePrefix lc ++ "Invalid reference: " ++ msg
  UndefinedTrait name lc -> linePrefix lc ++ "Trait '" ++ name ++ "' is not defined"
  MissingTraitMethod traitName typeName methodName lc -> 
    linePrefix lc ++ "Implementation of trait '" ++ traitName ++ "' for type '" ++ typeName ++ "' is missing required method '" ++ methodName ++ "'"
  UnexpectedTraitMethod traitName typeName methodName lc -> 
    linePrefix lc ++ "Implementation of trait '" ++ traitName ++ "' for type '" ++ typeName ++ "' has unexpected method '" ++ methodName ++ "' (not declared in trait)"
  where
    linePrefix (line, _) = "Line " ++ show (line + 1) ++ ": "
