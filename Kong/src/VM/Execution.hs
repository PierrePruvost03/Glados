{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module VM.Execution (exec) where

import DataStruct.VM (VMState(..), Stack)
import DataStruct.Bytecode.Value
import Control.Exception
import qualified Data.Vector as V
import qualified Data.Map as M
import VM.EnvGestion (mergeEnv)
import VM.Errors (ExecError (..))
import VM.Operations (applyOp, makeIntValue, makeBoolValue)

createList :: Stack -> Int -> ([Value], Stack)
createList stack n = f ([], stack) n
    where
        f r 0 = r
        f (_, []) _ = throw $ InvalidStackAccess
        f (l, (x:xs)) it = f (x : l, xs) (it - 1)

createStruct :: Stack -> [String] -> ([(String , Value)], Stack)
createStruct stack names = f ([], stack) names
    where
        f r [] = r
        f (_, []) _ = throw $ InvalidStackAccess
        f (l, vX:vXs) (nX:nXs) = f ((nX, vX):l, vXs) nXs

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
checkInstrution state@(VMState {stack = _ : _ : xs, ip}) (DoOp op) =
     applyOp state op >>= \x -> exec $ state {stack = VNumber x : xs, ip = ip + 1}

-- Call
checkInstrution s@(VMState {stack = ((VFunction symbols code):xs), env, ip}) Call =
    exec (s {code = code, stack = xs, env = mergeEnv env symbols, ip = 0}) >>= \case
        (VMState {stack = (x:_)}) -> exec $ s {stack = x:xs, ip = ip + 1}
        _ -> throwIO $ InvalidStackAccess
-- Var
checkInstrution s@(VMState {stack = (x:xs), env, ip}) (SetVar n) =
    exec $ s {env = M.insert n x env, stack = xs, ip = ip + 1}

-- List
checkInstrution s@(VMState {stack = (VList li c) : index : value :xs, ip}) SetList =
    exec $ s {stack = VList (li V.// [(makeIntValue index, value)]) c : xs, ip = ip + 1}
checkInstrution s@(VMState {stack = (VList li _) : index : xs, ip}) GetList = case li V.!? intIndex of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ AccessOutOfRange intIndex
    where intIndex = makeIntValue index
checkInstrution s@(VMState {stack, ip}) (CreateList n) = case createList stack n of
    (values, xs) -> exec $ s {stack = VList (V.fromList values) False : xs, ip = ip + 1}

-- Struct
checkInstrution s@(VMState {stack = VStruct struct c : value : xs, ip}) (SetStruct field) =
    exec $ s {stack = VStruct (M.insert field value struct) c : xs, ip = ip + 1}
checkInstrution s@(VMState {stack = VStruct struct _ : xs, ip}) (GetStruct field) = case struct M.!? field of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ InvalidStructAccess field
checkInstrution s@(VMState {stack, ip}) (CreateStruct l) = case createStruct stack l of
    (values, xs) -> exec $ s {stack = VStruct (M.fromList values) False : xs, ip = ip + 1}


-- Heap Management
checkInstrution s@(VMState {stack = (VRef addr : xs), heap, ip}) LoadRef = case heap V.!? addr of
    Just v -> exec $ s {stack = v : xs, ip = ip + 1}
    Nothing -> throwIO $ InvalidHeapAccess
checkInstrution s@(VMState {stack, heap, ip}) Alloc =
    exec $ s {stack = (VRef $ length heap) : stack, ip = ip + 1}
checkInstrution s@(VMState {stack = ref@(VRef addr) : v : xs, heap, ip}) StoreRef =
    exec $ s {stack = ref:xs, heap = heap V.// [(addr, v)], ip = ip + 1}


-- checkInstrution s@(VMState {stack, env, ip})
checkInstrution _ _ = throw $ UnknowInstruction
