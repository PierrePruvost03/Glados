{-# LANGUAGE LambdaCase #-}

module Interpreter.Primitives.Primitives
  ( primitiveList,
    primAdd,
    primSub,
    primMul,
    primDiv,
    primEq,
    primLt,
    primGt,
    primMod,
    primZero,
  )
where

import Data.List (transpose)
import DataStruct.Value (Value (..))
import Debug.Trace

primOp :: [Value] -> (Int -> Int -> Int) -> Int -> Either String Value
primOp [] _ base = Right $ VInt base
primOp [VInt a] f base = Right $ VInt $ f base a
primOp [VInt a, VInt b] f _ = Right $ VInt $ f a b
primOp (VInt a : VInt b : xs) f base = primOp (VInt (f a b) : xs) f base
primOp (a : _) _ _ = Left $ "expected integers, got " ++ show a

primAdd :: [Value] -> Either String Value
primAdd l = case primOp l (+) 0 of
  v@(Right _) -> v
  Left s -> Left $ "primAdd: " <> s

primSub :: [Value] -> Either String Value
primSub l = case primOp l (-) 0 of
  v@(Right _) -> v
  Left s -> Left $ "primSub: " <> s

primMul :: [Value] -> Either String Value
primMul l = case primOp l (*) 1 of
  v@(Right _) -> v
  Left s -> Left $ "primMul: " <> s

primDiv :: [Value] -> Either String Value
primDiv l@(_ : xs)
  | all f xs = case primOp l div 1 of
      v@(Right _) -> v
      Left s -> Left $ "primDiv: " <> s
  | otherwise = Left $ "primDiv: division by zero"
  where
    f (VInt 0) = False
    f _ = True
primDiv l = case primOp l div 1 of
  v@(Right _) -> v
  Left s -> Left $ "primDiv: " <> s

primEq :: [Value] -> Either String Value
primEq [a, b] = Right $ VBool (a == b)
primEq args = Left $ "primEq: expected two values, got " ++ show args

(<&&>) :: Either String Value -> Either String Value -> Either String Value
(<&&>) e@(Left _) _ = e
(<&&>) _ e@(Left _) = e
(<&&>) (Right (VBool a)) (Right (VBool b)) = Right $ VBool $ a && b
(<&&>) _ _ = Left "Bool evaluation: both element should be bool"

primEval :: (Ord a) => [a] -> (a -> a -> Bool) -> Either String Value
primEval [] _ = Left "expecting arguments, got none"
primEval [_] _ = Right $ VBool True
primEval (x : y : xs) f = Right (VBool (f x y)) <&&> primEval (y : xs) f

isNum :: Value -> Bool
isNum (VInt _) = True
isNum (VBool _) = True
isNum _ = False

isString :: Value -> Bool
isString (VString _) = True
isString _ = False

isList :: Value -> Bool
isList (VList _) = True
isList _ = False

areCompLists :: [Value] -> Bool
areCompLists l = all isList l && all areComparable (transpose (unwrap l))
  where
    unwrap [] = []
    unwrap (VList li : xs) = li : unwrap xs
    unwrap _ = []

areComparable :: [Value] -> Bool
areComparable l = all isNum l || all isString l || areCompLists l

safePrimLt :: [Value] -> Either String Value
safePrimLt l = case primEval l (<) of
  v@(Right _) -> v
  Left s -> Left $ "primLt: " <> s

primLt :: [Value] -> Either String Value
primLt l
  | areComparable l = safePrimLt l
  | otherwise = Left $ "primLt: elements are not comparable " <> show l

safePrimGt :: [Value] -> Either String Value
safePrimGt l = case primEval l (>) of
  v@(Right _) -> v
  Left s -> Left $ "primGt: " <> s

primGt :: [Value] -> Either String Value
primGt l
  | areComparable l = safePrimGt l
  | otherwise = Left $ "primGt: elements are not comparable " <> show l

primMod :: [Value] -> Either String Value
primMod [_, VInt 0] = Left "primMod: modulo by zero"
primMod [VInt a, VInt b] = Right $ VInt (a `mod` b)
primMod args = Left $ "primMod: expected two integers, got " ++ show args

primZero :: [Value] -> Either String Value
primZero [VInt 0] = Right $ VBool True
primZero [VInt _] = Right $ VBool False
primZero [VBool False] = Right $ VBool True
primZero [VBool _] = Right $ VBool False
primZero args = Left $ "primZero: expected one integer, got " ++ show args

primitiveList :: [(String, Value)]
primitiveList =
  [ ("+", VPrim "+" primAdd),
    ("-", VPrim "-" primSub),
    ("*", VPrim "*" primMul),
    ("div", VPrim "div" primDiv),
    ("eq?", VPrim "eq?" primEq),
    ("<", VPrim "<" primLt),
    (">", VPrim ">" primGt),
    ("mod", VPrim "mod" primMod),
    ("zero?", VPrim "zero?" primZero)
  ]
