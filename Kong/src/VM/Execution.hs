{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module VM.Execution
    (
    ) where

import DataStruct.VM (VMState(..), ExecError(..), ExecEnv, Heap, Stack, HeapAddr, initVMState)
import DataStruct.Bytecode.Number
import DataStruct.Bytecode.Op (Op(..), builtinOps, stringToOp, get, put)
import DataStruct.Bytecode.Utils (construct, constructList, putManyMany, getMany, getList)
import DataStruct.Bytecode.Value
import Control.Exception
import qualified Data.Vector as V
import qualified Data.Map as M
import VM.EnvGestion (mergeEnv)
import VM.Errors (ExecError (..))

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
makeBoolValue (VString string) = null string
makeBoolValue (VTuple (vector) _) = null vector
makeBoolValue (VArray (vector) _) = null vector
makeBoolValue (VVector (vector) _) = null vector
-- makeBoolValue VStruct String (M.Map String HeapAddr)
-- makeBoolValue VFunction [String] [Instr] Env
-- makeBoolValue VBuiltinOp Op
-- makeBoolValue VRef HeapAddr
makeBoolValue VEmpty = False
makeBoolValue _ = False

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
divOp (_, (VBool False)) = throw $ Err 2
divOp (_, (VChar '\0')) = throw $ Err 2
divOp (_, (VInt 0)) = throw $ Err 2
divOp (_, (VFloat 0)) = throw $ Err 2
divOp ((VBool a), (VBool b)) = pure $ VBool (a && b)
divOp ((VChar a), (VChar b)) = pure $ VChar (toEnum ((fromEnum a) + (fromEnum b))::Char)
divOp ((VInt a), (VInt b)) = pure $ VInt (a + b)
divOp ((VFloat a), (VFloat b)) = pure $ VFloat (a + b)
divOp (v1, v2) = throwIO $ InvalidOpTypeError (VNumber v1) (VNumber v2)

equalOp :: (Number, Number) -> IO Number
equalOp (a, b) = pure $ VBool (a == b)

applyOp :: VMState -> Op -> IO Number
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Add = addOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Sub = subOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Mul = mulOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Div = divOp $ compareTypes a b
applyOp (VMState {stack = (VNumber a: VNumber b: _)}) Equal = equalOp $ compareTypes a b
applyOp (VMState {stack = (v1: v2: _)}) _ = throwIO $ InvalidOpTypeError v1 v2
applyOp _ _ = throwIO $ InvalidStackAccess

exec :: VMState -> IO VMState
exec state@(VMState {code, ip}) = case code V.!? ip of
    Nothing -> throwIO $ ByteCodeOutOfRange
    Just instr -> checkInstrution state instr

checkInstrution :: VMState -> Instr -> IO VMState
checkInstrution s Ret = pure s
checkInstrution s@(VMState {ip}) Nop = exec $ s {ip = ip + 1}
checkInstrution state@(VMState {stack = xs, ip}) (Push value) =
    exec $ state {stack = value : xs, ip = ip + 1}
checkInstrution state@(VMState {ip}) (Jump n) = exec $ state {ip = ip + n}
checkInstrution state@(VMState {stack = x : xs, ip}) (JumpIfFalse n)
    | not $ makeBoolValue x = exec $ state {stack = xs, ip = ip + n}
    | otherwise = exec $ state {stack = xs, ip = ip + 1}
checkInstrution state@(VMState {stack = x : xs, ip}) (JumpIfTrue n)
    | makeBoolValue x = exec $ state {stack = xs, ip = ip + n}
    | otherwise = exec $ state {stack = xs, ip = ip + 1}
checkInstrution state@(VMState {stack = _ : _ : xs, ip}) (DoOp op) =
     applyOp state op >>= \x -> exec $ state {stack = VNumber x : xs, ip = ip + 1}
checkInstrution s@(VMState {stack, env, ip}) (PushEnv n) = case env M.!? n of
    Just v -> exec $ s {stack = (v : stack), ip = ip + 1}
    Nothing -> throwIO $ VarDoesNotExists n
checkInstrution s@(VMState {stack = ((VFunction symbols code):xs), env, ip}) Call =
    exec (s {code = code, stack = xs, env = mergeEnv env symbols, ip = 0}) >>=
        \(VMState {stack = (x:_)}) -> exec $ s {stack = x:xs, ip = ip + 1}
checkInstrution s@(VMState {stack = (x:xs), env, ip}) (SetVar n) =
    exec $ s {env = M.insert n x env, stack = xs, ip = ip + 1}
checkInstrution s@(VMState {stack = (x:xs), env, ip}) (SetVector n i) = case env M.!? n of
    Just (VVector v c) -> exec $ s {env = M.insert n (VVector (v V.// [(i, x)]) c) env, stack = xs, ip = ip + 1}
    Nothing -> throwIO $ VarDoesNotExists n
checkInstrution s@(VMState {stack = (x:xs), env, ip}) (SetArray n i) = case env M.!? n of
    Just (VArray v c) -> exec $ s {env = M.insert n (VArray (v V.// [(i, x)]) c) env, stack = xs, ip = ip + 1}
    Nothing -> throwIO $ VarDoesNotExists n
checkInstrution s@(VMState {stack = (x:xs), env, ip}) (SetTuple n i) = case env M.!? n of
    Just (VArray v c) -> exec $ s {env = M.insert n (VTuple (v V.// [(i, x)]) c) env, stack = xs, ip = ip + 1}
    Nothing -> throwIO $ VarDoesNotExists n
checkInstrution s@(VMState {stack = (VRef addr : xs), heap, ip}) LoadRef = case heap V.!? addr of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ InvalidHeapAccess

checkInstrution _ _ = throw $ UnknowInstruction
