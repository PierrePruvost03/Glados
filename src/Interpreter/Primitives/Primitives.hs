module Interpreter.Primitives.Primitives (
    primitiveList,
    primAdd,
    primSub,
    primMul,
    primDiv,
    primEq,
    primLt,
    primGt,
    primMod,
    primZero
) where

import DataStruct.Value (Value (..))


primAdd :: [Value] -> Either String Value
primAdd [VInt a, VInt b] = Right $ VInt (a + b)
primAdd args = Left $ "primAdd: expected two integers, got " ++ show args

primSub :: [Value] -> Either String Value
primSub [VInt a, VInt b] = Right $ VInt (a - b)
primSub args = Left $ "primSub: expected two integers, got " ++ show args

primMul :: [Value] -> Either String Value
primMul [VInt a, VInt b] = Right $ VInt (a * b)
primMul args = Left $ "primMul: expected two integers, got " ++ show args

primDiv :: [Value] -> Either String Value
primDiv [_, VInt 0] = Left "primDiv: division by zero"
primDiv [VInt a, VInt b] = Right $ VInt (a `div` b)
primDiv args = Left $ "primDiv: expected two integers, got " ++ show args

primEq :: [Value] -> Either String Value
primEq [a, b] = Right $ VBool (a == b)
primEq args = Left $ "primEq: expected two values, got " ++ show args

primLt :: [Value] -> Either String Value
primLt [VInt a, VInt b] = Right $ VBool (a < b)
primLt args = Left $ "primLt: expected two integers, got " ++ show args

primGt :: [Value] -> Either String Value
primGt [VInt a, VInt b] = Right $ VBool (a > b)
primGt args = Left $ "primGt: expected two integers, got " ++ show args

primMod :: [Value] -> Either String Value
primMod [_, VInt 0] = Left "primMod: modulo by zero"
primMod [VInt a, VInt b] = Right $ VInt (a `mod` b)
primMod args = Left $ "primMod: expected two integers, got " ++ show args

primZero :: [Value] -> Either String Value
primZero [VInt 0] = Right $ VBool True
primZero [VInt _] = Right $ VBool False
primZero args = Left $ "primZero: expected one integer, got " ++ show args

primitiveList :: [(String, Value)]
primitiveList =
  [ ("+", VPrim "+" primAdd)
  , ("-", VPrim "-" primSub)
  , ("*", VPrim "*" primMul)
  , ("div", VPrim "div" primDiv)
  , ("eq?", VPrim "eq?" primEq)
  , ("<", VPrim "<" primLt)
  , (">", VPrim ">" primGt)
  , ("mod", VPrim "mod" primMod)
  , ("zero?", VPrim "zero?" primZero)
  ]