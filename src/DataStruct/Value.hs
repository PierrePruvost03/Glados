{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.Value
  ( Value (..),
    Env,
    showValue,
    showTypeAndValue,
  )
where

import Data.Map.Strict (Map)
import DataStruct.Ast (Ast)

type Env = Map String Value

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VList [Value]
  | VLambda {vLParams :: [String], vLBody :: Ast, vLEnv :: Env}
  | VPrim {primName :: String, primImpl :: [Value] -> Either String Value}

-- For LISP-style output
showValue :: Value -> String
showValue (VInt i) = show i
showValue (VBool True) = "#t"
showValue (VBool False) = "#f"
showValue (VString s) = show s
showValue (VList xs) = '(' : unwords (map showValue xs) <> ")"
showValue (VLambda args _ _) = "#<lambda> " <> show args
showValue (VPrim _ _) = "#<primitive>"

showTypeAndValue :: Value -> String
showTypeAndValue v@(VInt _) = "Int " <> showValue v
showTypeAndValue v@(VBool _) = "Boolean " <> showValue v
showTypeAndValue v@(VString _) = "String " <> showValue v
showTypeAndValue (VList xs) = "List (" <> unwords (map showTypeAndValue xs) <> ")"
showTypeAndValue v@(VLambda {}) = showValue v
showTypeAndValue v@(VPrim _ _) = showValue v

-- For debugging
instance Show Value where
  show (VInt i) = "VInt " ++ show i
  show (VBool b) = "VBool " ++ show b
  show (VString s) = "VString " ++ show s
  show (VList xs) = "VList " ++ show xs
  show (VLambda params body _) = "VLambda " ++ show params ++ " " ++ show body ++ " <env>"
  show (VPrim name _) = "VPrim " ++ show name ++ " <function>"

instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VBool a) == (VBool b) = a == b
  (VString a) == (VString b) = a == b
  (VList a) == (VList b) = a == b
  (VLambda p1 b1 e1) == (VLambda p2 b2 e2) = p1 == p2 && b1 == b2 && e1 == e2
  (VPrim n1 _) == (VPrim n2 _) = n1 == n2
  _ == _ = False
