{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module VM.Execution (exec, createList) where

import DataStruct.VM (VMState(..))
import DataStruct.Bytecode.Value
import DataStruct.Bytecode.Number (Number(..))
import Control.Exception
import qualified Data.Vector as V
import qualified Data.Map as M
import VM.EnvGestion (mergeEnv)
import VM.Errors (ExecError (..))
import VM.Operations (applyOp)
import VM.Syscall (executeSyscall)
import VM.Cast (castNumber)
import VM.Utils
import Data.Maybe (fromMaybe)

exec :: VMState -> IO VMState
exec state@(VMState {code, ip}) = case code V.!? ip of
    Nothing -> throwIO $ ByteCodeOutOfRange
    Just instr -> checkInstrution state instr


checkInstrution :: VMState -> Instr -> IO VMState
checkInstrution s Ret = pure s
checkInstrution s@(VMState {ip}) Nop = exec $ s {ip = ip + 1}

-- Push
checkInstrution state@(VMState {stack = xs, ip}) (Push value) =
    exec $ state {stack = value : xs, ip = ip + 1}
checkInstrution s@(VMState {stack, env, ip}) (PushEnv n) = case env M.!? n of
    Just v -> exec $ s {stack = (v : stack), ip = ip + 1}
    Nothing -> throwIO $ VarDoesNotExists n

-- Jumps
checkInstrution state@(VMState {ip}) (Jump n) = exec $ state {ip = ip + n}
checkInstrution state@(VMState {stack = x : xs, ip}) (JumpIfFalse n)
    | not $ makeBoolValue x = exec $ state {stack = xs, ip = ip + n}
    | otherwise = exec $ state {stack = xs, ip = ip + 1}
checkInstrution state@(VMState {stack = x : xs, ip}) (JumpIfTrue n)
    | makeBoolValue x = exec $ state {stack = xs, ip = ip + n}
    | otherwise = exec $ state {stack = xs, ip = ip + 1}

-- Operations
checkInstrution s@(VMState {ip}) (DoOp op) = exec $ (applyOp s op) {ip = ip + 1}
checkInstrution s@(VMState {stack = (VNumber v : xs), ip}) (Cast t) =
    exec $ s {stack = (VNumber (castNumber v t) : xs), ip = ip + 1}
checkInstrution s@(VMState {stack = VList l : xs, ip}) Length =
    exec $ s {stack = VNumber (VInt (V.length l)) : xs, ip = ip + 1}

-- Call
checkInstrution s@(VMState {stack = ((VFunction symbols code):xs), env, heap, ip}) Call =
    exec (s {code = code, stack = xs, env = mergeEnv env symbols, ip = 0}) >>= \case
        (VMState {stack = (x:_), heap = r}) ->
            exec $ s {stack = x:xs, heap = mergeHeaps heap r, ip = ip + 1}
        _ -> throwIO $ InvalidStackAccess
-- Var
checkInstrution s@(VMState {stack = (x:xs), env, ip}) (SetVar n) =
    exec $ s {env = M.insert n x env, stack = xs, ip = ip + 1}

-- List
checkInstrution s@(VMState {stack = (VList li) : index : value :xs, ip}) SetList
    | intIndex < (V.length li) = exec $ s {stack = VList (li V.// [(intIndex, value)]) : xs, ip = ip + 1}
    | otherwise = throwIO $ AccessOutOfRange intIndex
    where intIndex = makeIntValue index
checkInstrution s@(VMState {stack = (VList li) : index : xs, ip}) GetList = case li V.!? intIndex of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ AccessOutOfRange intIndex
    where intIndex = makeIntValue index
checkInstrution s@(VMState {stack, ip}) (CreateList n) = case createList stack n of
    (values, xs) -> exec $ s {stack = VList (V.fromList values) : xs, ip = ip + 1}

-- List Management
checkInstrution s@(VMState {stack = VList l : v : xs, ip}) ListPush =
    exec $ s {stack = VList (V.snoc l v) : xs, ip = ip + 1}
checkInstrution s@(VMState {stack = VList l : xs, ip}) ListPop =
    exec $ s {stack = VList ( V.fromList [] `fromMaybe` (fst <$> V.unsnoc l)) : xs, ip = ip + 1}



-- Struct
checkInstrution s@(VMState {stack = VStruct struct : value : xs, ip}) (SetStruct field) =
    exec $ s {stack = VStruct (M.insert field value struct) : xs, ip = ip + 1}
checkInstrution s@(VMState {stack = VStruct struct : xs, ip}) (GetStruct field) = case struct M.!? field of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ InvalidStructAccess field
checkInstrution s@(VMState {stack, ip}) (CreateStruct l) = case createStruct stack l of
    (values, xs) -> exec $ s {stack = VStruct (M.fromList values) : xs, ip = ip + 1}


-- Heap Management
checkInstrution s@(VMState {stack = (VRef addr : xs), heap, ip}) LoadRef = case heap V.!? addr of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ InvalidHeapAccess
checkInstrution s@(VMState {stack, heap, ip}) Alloc =
    exec $ s {stack = (VRef $ length heap) : stack, ip = ip + 1, heap = V.snoc heap VEmpty}
checkInstrution s@(VMState {stack = ref@(VRef addr) : v : xs, heap, ip}) StoreRef =
    exec $ s {stack = ref:xs, heap = heap V.// [(addr, v)], ip = ip + 1}

-- Syscall
checkInstrution s (Syscall call) = executeSyscall call s >>= exec

-- Error
checkInstrution _ _ = throw $ UnknowInstruction
