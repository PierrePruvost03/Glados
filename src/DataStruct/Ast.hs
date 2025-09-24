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

import Parser.LineCount

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
  = AValue (LineCount, AstValue)
  | ASymbol (LineCount, AstSymbol)
  | AList (LineCount, AstList)
  | ADefine { name :: AstSymbol, value :: Ast } (LineCount, LineCount)
  | ALambdas AstLambda (LineCount, LineCount)
  | ACall { name :: String, args :: Ast } (LineCount, LineCount)
  | AIf { ifCond :: Ast, ifThen :: Ast, ifElse :: Ast } (LineCount, LineCount)
   deriving (Eq, Ord, Show)
