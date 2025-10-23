module VM.Errors (ExecError (..)) where

import Control.Exception
import DataStruct.Bytecode.Value (Value (..))

data ExecError
    = ByteCodeOutOfRange
    | InvalidOpTypeError Value Value
    | VarDoesNotExists String
    | AccessOutOfRange String Int
    | InvalidStackAccess
    | InvalidHeapAccess
    | UnknowInstruction

instance Show ExecError where
    show ByteCodeOutOfRange = "Bytecode access out of range"
    show (InvalidOpTypeError v1 v2) = "Cannot do operation on value \"" <> show v1 <> "\" and \"" <> show v2 <> "\""
    show (VarDoesNotExists n) = "Unknow variable " <> show n
    show (AccessOutOfRange n i) = "Unable to access var " <> show n <> "at index " <> show i
    show (InvalidStackAccess) = "Invalid Stack Access"
    show (InvalidHeapAccess) = "Invalid Heap Access"
    show (UnknowInstruction) = "Unknow instruction, this might be because of invalid stack value for existing instruction"

instance Exception ExecError
