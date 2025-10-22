{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module VM.Execution
    (
    ) where

import DataStruct.VM (VMState(..), ExecError(..), Env, Heap, Stack, HeapAddr, initVMState)
import DataStruct.Bytecode.Number
import DataStruct.Bytecode.Op (Op(..), builtinOps, stringToOp, get, put)
import DataStruct.Bytecode.Utils (construct, constructList, putManyMany, getMany, getList)
import DataStruct.Bytecode.Value
import Control.Exception
import Data.Vector ((!?))
import Data.Map ((!))

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
makeBoolValue (VString "") = False
makeBoolValue (VString _) = True
-- makeBoolValue VTuple (V.Vector Value)
-- makeBoolValue VArray (V.Vector Value) Bool
-- makeBoolValue VVector (V.Vector Value)
-- makeBoolValue VList (V.Vector Value)
-- makeBoolValue VStruct String (M.Map String HeapAddr)
-- makeBoolValue VFunction [String] [Instr] Env
-- makeBoolValue VBuiltinOp Op
-- makeBoolValue VRef HeapAddr
makeBoolValue VEmpty = False

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
addOp ((VBool a), (VBool b)) = VBool $ (a /= b)
addOp ((VChar a), (VChar b)) = VChar $ (toEnum ((fromEnum a) + (fromEnum b))::Char)
addOp ((VInt a), (VInt b)) = VInt $ a + b
addOp ((VFloat a), (VFloat b)) = VFloat $ a + b

subOp :: (Number, Number) -> Number
subOp ((VBool a), (VBool b)) = VBool $ (a == b)
subOp ((VChar a), (VChar b)) = VChar $ (toEnum ((fromEnum a) - (fromEnum b))::Char)
subOp ((VInt a), (VInt b)) = VInt $ a - b
subOp ((VFloat a), (VFloat b)) = VFloat $ a - b

mulOp :: (Number, Number) -> Number
mulOp ((VBool a), (VBool b)) = VBool $ (a && b)
mulOp ((VChar a), (VChar b)) = VChar $ (toEnum ((fromEnum a) * (fromEnum b))::Char)
mulOp ((VInt a), (VInt b)) = VInt $ a * b
mulOp ((VFloat a), (VFloat b)) = VFloat $ a * b

divOp :: (Number, Number) -> Number
divOp ((VBool a), (VBool False)) = throw
divOp ((VChar a), (VChar '\0')) = throw
divOp ((VInt a), (VInt 0)) = throw
divOp ((VFloat a), (VFloat 0)) = throw
divOp ((VBool a), (VBool b)) = VBool $ (a && b)
divOp ((VChar a), (VChar b)) = VChar $ (toEnum ((fromEnum a) + (fromEnum b))::Char)
divOp ((VInt a), (VInt b)) = VInt $ a + b
divOp ((VFloat a), (VFloat b)) = VFloat $ a + b

equalOp :: (Number, Number) -> Number
equalOp (a, b) = VBool $ (a == b)

applyOp :: VMState -> Op -> IO Number
applyOp VMState {stack = (VNumber a: VNumber b:xs)} Add = pure $ addOp $ compareTypes a b
applyOp VMState {stack = (VNumber a: VNumber b:xs)} Sub = pure $ subOp $ compareTypes a b
applyOp VMState {stack = (VNumber a: VNumber b:xs)} Mul = pure $ mulOp $ compareTypes a b
applyOp VMState {stack = (VNumber a: VNumber b:xs)} Div = pure $ divOp $ compareTypes a b
applyOp VMState {stack = (VNumber a: VNumber b:xs)} Equal = pure $ equalOp $ compareTypes a b

exec :: VMState -> IO VMState
exec state@(VMState s _ _ code ip) = case code!?ip of
    Nothing -> throw $ Err 1
    Just instr -> checkInstrution state instr

checkInstrution :: VMState -> Instr -> IO VMState
checkInstrution state@(VMState {stack = xs, ip}) (Push value) =
    exec $ state {stack = value : xs, ip = ip + 1}
checkInstrution state@(VMState {stack, ip}) (DoOp op) =
     applyOp state op >>= \x -> exec $ state {stack = VNumber x : stack, ip = ip + 1}
checkInstrution s@(VMState { stack, env, ip }) (PushEnv n) =
    exec $ s {stack = ((env!n) : stack), ip = ip + 1}

-- calculate :: Op -> Value -> Value -> Value
-- -- works differently with numbers and with lists
-- calculate Add (VNumber v1) (VNumber v2) = VNumber $ v1 + v2
-- calculate Sub (VNumber v1) (VNumber v2) = VNumber $ v1 - v2
-- calculate Mul (VNumber v1) (VNumber v2) = VNumber $ v1 * v2
-- calculate Div (VNumber v1) (VNumber v2) = VNumber $ v1 `div` v2
-- calculate Equal (VNumber v1) (VNumber v2) = VNumber $ v1 + v2
-- -- only works with numbers
-- calculate Lt (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 < v2
-- calculate Gt (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 > v2
-- calculate Le (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 <= v2
-- calculate Ge (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 >= v2
-- calculate Ne (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 /= v2
-- -- transforms non-bool into bool before op
-- calculate And v1 v2 = VBool $ (makeBoolValue v1) && (makeBoolValue v2)
-- calculate Or v1 v2 = VBool $ (makeBoolValue v1) || (makeBoolValue v2)
-- -- "not" operator only takes 1 argument, not sure how to handle this case
-- -- calculate Not

-- checkInstrution s@
checkInstrution state@(VMState {stack = xs, ip = n}) (Push value) = exec $ state {stack = value : xs, ip = n + 1}
checkInstrution state (DoOp op) = exec $ applyOp state op
checkInstrution state@(VMState {ip = n}) (Nop) = exec $ state {ip = n + 1}

-- checkOp :: VMState -> Op -> IO VMState
-- checkOp state@(VMState {stack = (a:b:xs)}) Add = state {stack = (builtinAdd a b : xs)}

-- instrRandom :: VMState -> IO VMState



-- exec :: Env -> [Instr] -> Stack -> Either String Value
-- exec _ [] _ = Left "Error: ending instruction set without a return"
-- exec e (Push value : is) st = exec e is $ [value] <> st
-- -- exec e (PushEnv String : is) _ =
-- -- exec _ (Call : is) _ =
-- exec _ (Ret : _) (value : _) = Right value
-- exec _ (Ret : _) [] = Left "Error: no value to return"
-- -- exec _ (Nop : is) _ =
-- -- exec _ (SetVar String : is) _ =
-- -- exec _ (SetArray Int : is) _ =
-- -- exec _ (SetVector Int : is) _ =
-- -- exec _ (SetStruct String : is) _ =
-- -- exec _ (SetTuple Int : is) _ =
-- -- exec _ (GetArray Int : is) _ =
-- -- exec _ (ArrayGet : is) _ =
-- -- exec _ (GetVector Int : is) _ =
-- -- exec _ (GetStruct String : is) _ =
-- -- exec _ (GetTuple Int : is) _ =
-- -- exec _ (Jump Int : is) _ =
-- -- exec _ (JumpIfFalse Int : is) _ =
-- -- exec _ (JumpIfTrue Int : is) _ =
-- exec e (DoOp op : is) (value1 : value2 : st) = exec e is $ [calculate op value1 value2] <> st
-- -- exec _ (PushLambda [String] [Instr] : is) _ =
-- -- exec _ (Alloc : is) _ =
-- -- exec _ (LoadRef : is) _ =
-- -- exec _ (StoreRef : is) _ =
