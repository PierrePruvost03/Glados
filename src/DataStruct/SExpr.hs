module DataStruct.SExpr
  ( SExpr (..),
  )
where

import Parser

data SExpr
  = SInt (LineCount, Int)
  | SBool (LineCount, Bool)
  | SString (LineCount, String)
  | SSymbol (LineCount, String)
  | SList (LineCount, [SExpr])
  deriving (Eq, Ord, Show)
