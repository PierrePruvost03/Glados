module VM.Errors (ExecError (..)) where

import Control.Exception
import DataStruct.Bytecode.Value (Value (..))

data ExecError
    = ByteCodeOutOfRange
    | InvalidOpTypeError Value Value
    | VarDoesNotExists String
    | AccessOutOfRange Int
    | InvalidStackAccess
    | InvalidHeapAccess
    | UnknowInstruction
    | InvalidIntConversion
    | InvalidCharConversion
    | InvalidStructAccess String
    | ImpossibleDivsionByZero
    | ExitException Int

instance Show ExecError where
    show ByteCodeOutOfRange = "Bytecode access out of range"
    show (InvalidOpTypeError v1 v2) = "Cannot do operation on value \"" <> show v1 <> "\" and \"" <> show v2 <> "\""
    show (VarDoesNotExists n) = "Unknow variable " <> show n
    show (AccessOutOfRange i) = "Unable to access list at index " <> show i
    show (InvalidStackAccess) = "Invalid Stack Access"
    show (InvalidHeapAccess) = "Invalid Heap Access"
    show (UnknowInstruction) = "Unknow instruction, this might be because of invalid stack value for existing instruction"
    show (InvalidIntConversion) = "Invalid Int Conversion, value cannot be computed as integer"
    show (InvalidCharConversion) = "Invalid Char Conversion, value cannot be computed as character"
    show (InvalidStructAccess s) = "Invalid struct access on field" <> show s
    show (ImpossibleDivsionByZero) = "Division by zero is an impossible operation"
    show _ = ""

instance Exception ExecError
