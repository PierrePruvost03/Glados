{-# LANGUAGE LambdaCase #-}

module VM.Execution
    (
    ) where

import DataStruct.VM (VMState(..), Env, Heap, Stack, HeapAddr, initVMState)
import DataStruct.Bytecode.Number
import DataStruct.Bytecode.Op (Op(..), builtinOps, stringToOp, get, put)
import DataStruct.Bytecode.Utils (construct, constructList, putManyMany, getMany, getList)
import DataStruct.Bytecode.Value

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

calculate :: Op -> Value -> Value -> Value
-- works differently with numbers and with lists
calculate Add (VNumber v1) (VNumber v2) = VNumber $ v1 + v2
calculate Sub (VNumber v1) (VNumber v2) = VNumber $ v1 - v2
calculate Mul (VNumber v1) (VNumber v2) = VNumber $ v1 * v2
calculate Div (VNumber v1) (VNumber v2) = VNumber $ v1 `div` v2 
calculate Equal (VNumber v1) (VNumber v2) = VNumber $ v1 + v2
-- only works with numbers
calculate Lt (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 < v2
calculate Gt (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 > v2
calculate Le (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 <= v2
calculate Ge (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 >= v2
calculate Ne (VNumber v1) (VNumber v2) = VNumber $ VBool $ v1 /= v2
-- transforms non-bool into bool before op
calculate And v1 v2 = VBool $ (makeBoolValue v1) && (makeBoolValue v2)
calculate Or v1 v2 = VBool $ (makeBoolValue v1) || (makeBoolValue v2)
-- "not" operator only takes 1 argument, not sure how to handle this case
-- calculate Not

exec :: Env -> [Instr] -> Stack -> Either String Value
exec _ [] _ = Left "Error: ending instruction set without a return"
exec e (Push value : is) st = exec e is $ [value] <> st
-- exec e (PushEnv String : is) _ =
-- exec _ (Call : is) _ =
exec _ (Ret : _) (value : _) = Right value
exec _ (Ret : _) [] = Left "Error: no value to return"
-- exec _ (Nop : is) _ =
-- exec _ (SetVar String : is) _ =
-- exec _ (SetArray Int : is) _ =
-- exec _ (SetVector Int : is) _ =
-- exec _ (SetStruct String : is) _ =
-- exec _ (SetTuple Int : is) _ =
-- exec _ (GetArray Int : is) _ =
-- exec _ (ArrayGet : is) _ =
-- exec _ (GetVector Int : is) _ =
-- exec _ (GetStruct String : is) _ =
-- exec _ (GetTuple Int : is) _ =
-- exec _ (Jump Int : is) _ =
-- exec _ (JumpIfFalse Int : is) _ =
-- exec _ (JumpIfTrue Int : is) _ =
exec e (DoOp op : is) (value1 : value2 : st) = exec e is $ [calculate op value1 value2] <> st
-- exec _ (PushLambda [String] [Instr] : is) _ =
-- exec _ (Alloc : is) _ =
-- exec _ (LoadRef : is) _ =
-- exec _ (StoreRef : is) _ =
