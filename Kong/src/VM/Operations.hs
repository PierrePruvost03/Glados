module VM.Operations (
    applyOp,
) where

import DataStruct.Bytecode.Value (Value(..))
import DataStruct.Bytecode.Op (Op(..))
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.VM (VMState(..))
import VM.Errors (ExecError(..))
import Control.Exception
import VM.Utils (makeBoolValue)

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

addOp :: (Number, Number) -> Number
addOp ((VBool a), (VBool b)) = VBool (a /= b)
addOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) + (fromEnum b))::Char)
addOp ((VInt a), (VInt b)) = VInt (a + b)
addOp ((VFloat a), (VFloat b)) = VFloat (a + b)
addOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

subOp :: (Number, Number) -> Number
subOp ((VBool a), (VBool b)) = VBool (a == b)
subOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) - (fromEnum b))::Char)
subOp ((VInt a), (VInt b)) = VInt (a - b)
subOp ((VFloat a), (VFloat b)) = VFloat (a - b)
subOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

mulOp :: (Number, Number) -> Number
mulOp ((VBool a), (VBool b)) = VBool (a && b)
mulOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) * (fromEnum b))::Char)
mulOp ((VInt a), (VInt b)) = VInt (a * b)
mulOp ((VFloat a), (VFloat b)) = VFloat (a * b)
mulOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

divOp :: (Number, Number) -> Number
divOp (_, (VBool False)) = throw $ ImpossibleDivsionByZero
divOp (_, (VChar '\0')) = throw $ ImpossibleDivsionByZero
divOp (_, (VInt 0)) = throw $ ImpossibleDivsionByZero
divOp (_, (VFloat 0)) = throw $ ImpossibleDivsionByZero
divOp ((VBool a), (VBool b)) = VBool (a && b)
divOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) + (fromEnum b))::Char)
divOp ((VInt a), (VInt b)) = VInt (a + b)
divOp ((VFloat a), (VFloat b)) = VFloat (a + b)
divOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

equalOp :: (Number, Number) -> Number
equalOp (a, b) = VBool (a == b)

lessThanOp :: (Number, Number) -> Number
lessThanOp (a, b) = VBool (a < b)

greaterThanOp :: (Number, Number) -> Number
greaterThanOp (a, b) = VBool (a > b)

lessEqualOp :: (Number, Number) -> Number
lessEqualOp (a, b) = VBool (a <= b)

greaterEqualOp :: (Number, Number) -> Number
greaterEqualOp (a, b) = VBool (a >= b)

notEqualOp :: (Number, Number) -> Number
notEqualOp (a, b) = VBool (a /= b)

andOp :: (Number, Number) -> Number
andOp (a, b) = VBool (makeBoolValue (VNumber a) && makeBoolValue (VNumber b))

orOp :: (Number, Number) -> Number
orOp (a, b) = VBool (makeBoolValue (VNumber a) || makeBoolValue (VNumber b))

notOp :: Number -> Number
notOp n = VBool (not (makeBoolValue (VNumber n)))

applyOp :: VMState -> Op -> VMState
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Add =
     s {stack = VNumber (addOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Sub =
    s {stack = VNumber (subOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Mul =
     s {stack =  VNumber (mulOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Div =
     s {stack = VNumber (divOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Equal =
     s {stack = VNumber (equalOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Lt =
     s {stack = VNumber (lessThanOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Gt =
     s {stack = VNumber (greaterThanOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Le =
     s {stack = VNumber (lessEqualOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Ge =
     s {stack = VNumber (greaterEqualOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Ne =
     s {stack = VNumber (notEqualOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) And =
     s {stack = VNumber (andOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Or =
     s {stack = VNumber (orOp $ compareTypes a b) : xs}
applyOp s@(VMState {stack = (VNumber a: xs)}) Not =
     s {stack = VNumber (notOp a) : xs}
applyOp (VMState {stack = (v1: v2: _)}) _ = throw $ InvalidOpTypeError v1 v2
applyOp _ _ = throw $ InvalidStackAccess
