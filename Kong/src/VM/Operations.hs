module VM.Operations (
    applyOp,
) where

import DataStruct.Bytecode.Value (Value(..))
import DataStruct.Bytecode.Op (Op(..))
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.VM (VMState(..))
import VM.Errors (ExecError(..))
import Control.Exception
import VM.Utils (makeBoolValue, makeIntValue)

compareTypes :: Number -> Number -> (Number, Number)
compareTypes (VBool a) (VBool b) = (VBool a, VBool b)
compareTypes (VBool a) (VChar b) = (VChar a2, VChar b) where a2 = toEnum (fromEnum a)::Char
compareTypes (VBool a) (VInt b) = (VInt a2, VInt b) where a2 = fromIntegral (fromEnum a)
compareTypes (VBool a) (VFloat b) = (VFloat a2, VFloat b) where a2 = fromIntegral (fromEnum a)::Double
compareTypes (VBool a) (VLong b) = (VLong a2, VLong b) where a2 = fromIntegral (fromEnum a)
compareTypes (VBool a) (VUInt b) = (VUInt a2, VUInt b) where a2 = fromIntegral (fromEnum a)
compareTypes a (VBool b) = (a2, b2) where (b2, a2) = compareTypes (VBool b) a
compareTypes (VChar a) (VChar b) = (VChar a, VChar b)
compareTypes (VChar a) (VInt b) = (VInt a2, VInt b) where a2 = fromIntegral (fromEnum a)
compareTypes (VChar a) (VFloat b) = (VFloat a2, VFloat b) where a2 = fromIntegral (fromEnum a)::Double
compareTypes (VChar a) (VLong b) = (VLong a2, VLong b) where a2 = fromIntegral (fromEnum a)
compareTypes (VChar a) (VUInt b) = (VUInt a2, VUInt b) where a2 = fromIntegral (fromEnum a)
compareTypes a (VChar b) = (a2, b2) where (b2, a2) = compareTypes (VChar b) a
compareTypes (VInt a) (VInt b) = (VInt a, VInt b)
compareTypes (VInt a) (VFloat b) = (VFloat a2, VFloat b) where a2 = fromIntegral a::Double
compareTypes (VInt a) (VLong b) = (VLong a2, VLong b) where a2 = fromIntegral a
compareTypes (VInt a) (VUInt b) = (VUInt a2, VUInt b) where a2 = fromIntegral (max 0 a)
compareTypes a (VInt b) = (a2, b2) where (b2, a2) = compareTypes (VInt b) a
compareTypes (VFloat a) (VFloat b) = (VFloat a, VFloat b)
compareTypes (VFloat a) (VLong b) = (VFloat a, VFloat b2) where b2 = fromIntegral b::Double
compareTypes (VFloat a) (VUInt b) = (VFloat a, VFloat b2) where b2 = fromIntegral b::Double
compareTypes a (VFloat b) = (a2, b2) where (b2, a2) = compareTypes (VFloat b) a
compareTypes (VLong a) (VLong b) = (VLong a, VLong b)
compareTypes (VLong a) (VUInt b) = (VLong a, VLong b2) where b2 = fromIntegral b
compareTypes a (VLong b) = (a2, b2) where (b2, a2) = compareTypes (VLong b) a
compareTypes (VUInt a) (VUInt b) = (VUInt a, VUInt b)

addOp :: (Number, Number) -> Number
addOp ((VBool a), (VBool b)) = VBool (a /= b)
addOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) + (fromEnum b))::Char)
addOp ((VInt a), (VInt b)) = VInt (a + b)
addOp ((VFloat a), (VFloat b)) = VFloat (a + b)
addOp ((VLong a), (VLong b)) = VLong (a + b)
addOp ((VUInt a), (VUInt b)) = VUInt (a + b)
addOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

subOp :: (Number, Number) -> Number
subOp ((VBool a), (VBool b)) = VBool (a == b)
subOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) - (fromEnum b))::Char)
subOp ((VInt a), (VInt b)) = VInt (a - b)
subOp ((VFloat a), (VFloat b)) = VFloat (a - b)
subOp ((VLong a), (VLong b)) = VLong (a - b)
subOp ((VUInt a), (VUInt b)) = VUInt (a - b)
subOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

mulOp :: (Number, Number) -> Number
mulOp ((VBool a), (VBool b)) = VBool (a && b)
mulOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) * (fromEnum b))::Char)
mulOp ((VInt a), (VInt b)) = VInt (a * b)
mulOp ((VFloat a), (VFloat b)) = VFloat (a * b)
mulOp ((VLong a), (VLong b)) = VLong (a * b)
mulOp ((VUInt a), (VUInt b)) = VUInt (a * b)
mulOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)

divOp :: (Number, Number) -> Number
divOp (_, (VBool False)) = throw ImpossibleDivsionByZero
divOp (_, (VChar '\0')) = throw ImpossibleDivsionByZero
divOp (_, (VInt 0)) = throw ImpossibleDivsionByZero
divOp (_, (VFloat 0)) = throw ImpossibleDivsionByZero
divOp (_, (VLong 0)) = throw $ ImpossibleDivsionByZero
divOp (_, (VUInt 0)) = throw $ ImpossibleDivsionByZero
divOp ((VBool a), (VBool b)) = VBool (a && b)
divOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) `div` (fromEnum b))::Char)
divOp ((VInt a), (VInt b)) = VInt (a `div` b)
divOp ((VFloat a), (VFloat b)) = VFloat (a / b)
divOp ((VLong a), (VLong b)) = VLong (a `div` b)
divOp ((VUInt a), (VUInt b)) = VUInt (a `div` b)
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

modOp :: (Number, Number) -> Number
modOp (_, (VBool False)) = throw ImpossibleModulusByZero
modOp (_, (VChar '\0')) = throw ImpossibleModulusByZero
modOp (_, (VInt 0)) = throw ImpossibleModulusByZero
modOp (_, (VFloat 0)) = throw ImpossibleModulusByZero
modOp ((VBool a), (VBool b)) = VBool (a /= b)
modOp ((VChar a), (VChar b)) = VChar (toEnum ((fromEnum a) `mod` (fromEnum b))::Char)
modOp ((VInt a), (VInt b)) = VInt (a `mod` b)
modOp ((VFloat a), (VFloat b)) = VFloat (a - b * fromIntegral (floor (a / b) :: Integer))
modOp (v1, v2) = throw $ InvalidOpTypeError (VNumber v1) (VNumber v2)


-- bNotOp :: Number -> Number
-- bNotOp a = complement a

-- xorOp :: (Number, Number) -> Number
-- xorOp (a, b) = a `xor` b

-- bitwiseLOp :: (Number, Number) -> Number
-- bitwiseLOp (a, b) = shiftL a (makeIntValue b)

-- BitwiseROp :: (Number, Number) -> Number
-- bitwiseROp (a, b) = shiftR a (makeIntValue b)

-- bOrOp :: (Number, Number) -> Number
-- bOrOp (a, b) = a .|. b

-- bAndOp :: (Number, Number) -> Number
-- bAndOp (a, b) =


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
applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Mod =
     s {stack = VNumber (modOp $ compareTypes a b) : xs}
-- applyOp (VMState {stack = VNumber a: xs}) BNot =
--      s {stack = VNumber (bNotOp a) : xs}
-- applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) Xor =
--      s {stack = VNumber (xorOp $ compareTypes a b) : xs}
-- applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) BitwiseL =
--      s {stack = VNumber (bitwiseLOp $ compareTypes a b) : xs}
-- applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) BitwiseR =
--      s {stack = VNumber (bitwiseROp $ compareTypes a b) : xs}
-- applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) BOr =
--      s {stack = VNumber (bOrOp $ compareTypes a b) : xs}
-- applyOp s@(VMState {stack = (VNumber a: VNumber b: xs)}) BAnd =
--      s {stack = VNumber (bAndOp $ compareTypes a b) : xs}
applyOp (VMState {stack = (v1: v2: _)}) _ = throw $ InvalidOpTypeError v1 v2
applyOp _ _ = throw $ InvalidStackAccess
