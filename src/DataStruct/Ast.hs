module DataStruct.Ast
  ( AstInt
  , AstSymbol
  , AstList
  , AstBool
  , AstLambda(..)
  , AstBoolLambda(..)
  , AstValue(..)
  , Ast(..)
  ) where

import LineCount from Parser (LineCount)

type AstInt = Int

type AstSymbol = String

type AstList = [Ast]

type AstBool = Bool

data AstLambda = AstLambda AstList Ast deriving (Eq, Ord, Show)

data AstBoolLambda
    = AstBLBool AstBool
    | AstBLLambda AstLambda
    -- ACall
    deriving (Eq, Ord, Show)

{--
data AstType
  = AInt AstInt
  | ABool AstBool
  | ACustom String -- pour l'instant
--}

data AstValue
    = AstInteger AstInt
    | AstBool AstBool
    | AstString String
    deriving (Eq, Ord, Show)

data Ast
  = AValue AstValue LineCount
  | ASymbol AstSymbol LineCount
  | AList AstList LineCount
  | ADefine { name :: AstSymbol, value :: Ast } LineCount
  | ALambdas AstLambda LineCount
  | ACall { name :: String, args :: Ast } LineCount
  | AIf { ifCond :: Ast, ifThen :: Ast, ifElse :: Ast } LineCount
   deriving (Eq, Ord, Show)
