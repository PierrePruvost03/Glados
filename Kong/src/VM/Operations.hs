module VM.Operations (
    applyOp,
    makeBoolValue,
    makeIntValue
) where

import Data.Char (digitToInt)
import DataStruct.Bytecode.Value (Value(..))
import DataStruct.Bytecode.Op (Op(..))
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.VM (VMState(..))
import VM.Errors (ExecError(..))
import Control.Exception

makeBoolValue :: Value -> Bool
makeBoolValue (VNumber (VBool value)) = value
makeBoolValue (VNumber (VInt n))
    | n > 0 = True
    | otherwise = False
makeBoolValue (VNumber (VChar '\0')) = False
makeBoolValue (VNumber (VChar _)) = True
makeBoolValue (VNumber (VFloat n))
    | n > 0 = True
    | otherwise = False
makeBoolValue (VList (list) _) = null list
-- makeBoolValue VStruct String (M.Map String HeapAddr)
-- makeBoolValue VFunction [String] [Instr] Env
-- makeBoolValue VBuiltinOp Op
-- makeBoolValue VRef HeapAddr
makeBoolValue VEmpty = False
makeBoolValue _ = False

makeIntValue :: Value -> Int
makeIntValue (VNumber (VInt i)) = i
makeIntValue (VNumber (VChar i)) = digitToInt i
makeIntValue (VNumber (VBool i)) = fromEnum i
makeIntValue _ = throw $ InvalidIntConversion

compareTypes :: Number -> Number -> (Number, Number)
compareTypes (VBool a) (VBool b) = (VBool a, VBool b)
compareTypes (VBool a) (VChar b) = (VChar a2, VChar b) where a2 = toEnum (fromEnum a)::Char
compareTypes (VBool a) (VInt b) = (VInt a2, VInt b) where a2 = fromEnum a
compareTypes (VBool a) (VFloat b) = (VFloat a2, VFloat b) where a2 = fromIntegral (fromEnum a)::Double
compareTypes a (VBool b) = (a2, b2) where (b2, a2) = compareTypes (VBool b) a
compareTypes (VChar a) (VChar b) = (VChar a, VChar b)
compareTypes (VChar a) (VInt b) = (VInt a2, VInt b) where a2 = fromEnum a
compareTypes (VChar a) (VFloat b) = (VFloat a2, VFloat b) where a2 = fromIntegral (fromEnum a)::Double
compareTypes a (VChar b) = (a2, b2) where (b2, a2) = compareTypes (VChar b) a
compareTypes (VInt a) (VInt b) = (VInt a, VInt b)
compareTypes (VInt a) (VFloat b) = (VFloat a2, VFloat b) where a2 = fromIntegral a::Double
compareTypes a (VInt b) = (a2, b2) where (b2, a2) = compareTypes (VInt b) a
compareTypes (VFloat a) (VFloat b) = (VFloat a, VFloat b)

addOp :: (Number, Number) -> IO Number
addOp ((VBool a), (VBool b)) = pure $ VBool (a /= b)
addOp ((VChar a), (VChar b)) = pure $ VChar (toEnum ((fromEnum a) + (fromEnum b))::Char)
addOp ((VInt a), (VInt b)) = pure $ VInt (a + b)
addOp ((VFloat a), (VFloat b)) = pure $ VFloat (a + b)
addOp (v1, v2) = throwIO $ InvalidOpTypeError (VNumber v1) (VNumber v2)

subOp :: (Number, Number) -> IO Number
subOp ((VBool a), (VBool b)) = pure $ VBool (a == b)
subOp ((VChar a), (VChar b)) = pure $ VChar (toEnum ((fromEnum a) - (fromEnum b))::Char)
subOp ((VInt a), (VInt b)) = pure $ VInt (a - b)
subOp ((VFloat a), (VFloat b)) = pure $ VFloat (a - b)
subOp (v1, v2) = throwIO $ InvalidOpTypeError (VNumber v1) (VNumber v2)

mulOp :: (Number, Number) -> IO Number
mulOp ((VBool a), (VBool b)) = pure $ VBool (a && b)
mulOp ((VChar a), (VChar b)) = pure $ VChar (toEnum ((fromEnum a) * (fromEnum b))::Char)
mulOp ((VInt a), (VInt b)) = pure $ VInt (a * b)
mulOp ((VFloat a), (VFloat b)) = pure $ VFloat (a * b)
mulOp (v1, v2) = throwIO $ InvalidOpTypeError (VNumber v1) (VNumber v2)

divOp :: (Number, Number) -> IO Number
divOp (_, (VBool False)) = throwIO $ ImpossibleDivsionByZero
divOp (_, (VChar '\0')) = throwIO $ ImpossibleDivsionByZero
divOp (_, (VInt 0)) = throwIO $ ImpossibleDivsionByZero
divOp (_, (VFloat 0)) = throwIO $ ImpossibleDivsionByZero
divOp ((VBool a), (VBool b)) = pure $ VBool (a && b)
divOp ((VChar a), (VChar b)) = pure $ VChar (toEnum ((fromEnum a) + (fromEnum b))::Char)
divOp ((VInt a), (VInt b)) = pure $ VInt (a + b)
divOp ((VFloat a), (VFloat b)) = pure $ VFloat (a + b)
divOp (v1, v2) = throwIO $ InvalidOpTypeError (VNumber v1) (VNumber v2)

equalOp :: (Number, Number) -> IO Number
equalOp (a, b) = pure $ VBool (a == b)

lessThanOp :: (Number, Number) -> IO Number
lessThanOp (a, b) = pure $ VBool (a < b)

greaterThanOp :: (Number, Number) -> IO Number
greaterThanOp (a, b) = pure $ VBool (a > b)

lessEqualOp :: (Number, Number) -> IO Number
lessEqualOp (a, b) = pure $ VBool (a <= b)

greaterEqualOp :: (Number, Number) -> IO Number
greaterEqualOp (a, b) = pure $ VBool (a >= b)

notEqualOp :: (Number, Number) -> IO Number
notEqualOp (a, b) = pure $ VBool (a /= b)

andOp :: (Number, Number) -> IO Number
andOp (a, b) = pure $ VBool (makeBoolValue (VNumber a) && makeBoolValue (VNumber b))

orOp :: (Number, Number) -> IO Number
orOp (a, b) = pure $ VBool (makeBoolValue (VNumber a) || makeBoolValue (VNumber b))

notOp :: Number -> IO Number
notOp n = pure $ VBool (not (makeBoolValue (VNumber n)))

applyOp :: VMState -> Op -> IO Number
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Add = addOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Sub = subOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Mul = mulOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Div = divOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Equal = equalOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Lt = lessThanOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Gt = greaterThanOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Le = lessEqualOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Ge = greaterEqualOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Ne = notEqualOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) And = andOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Or = orOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: _)}) Not = notOp a
applyOp (VMState {stack = (v1: v2: _)}) _ = throwIO $ InvalidOpTypeError v1 v2
applyOp _ _ = throwIO $ InvalidStackAccess
