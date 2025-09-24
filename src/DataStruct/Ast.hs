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
  = AValue AstValue
  | ASymbol AstSymbol
  | AList AstList
  | ADefine { name :: AstSymbol, value :: Ast }
  | ALambdas AstLambda
  | ACall { name :: String, args :: Ast }
  | AIf { ifCond :: Ast, ifThen :: Ast, ifElse :: Ast }
   deriving (Eq, Ord, Show)
