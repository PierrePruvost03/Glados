module DataStruct.SExpr
  ( SExpr (..),
  )
where

import Parser

data SExpr
  = SInt (LineCount, Int)
  | SBool (LineCount, Bool)
  | SSymbol (LineCount, String)
  | SList (LineCount, [SExpr])
  deriving (Eq, Ord, Show)
