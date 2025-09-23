module DataStruct.SExpr (
    SExpr (SInt, SSymbol, SList),
    ) where

import Parser

data SExpr = SInt (LineCount, Int)
            | SSymbol (LineCount, String)
            | SList (LineCount, [SExpr])
        deriving (Eq, Ord, Show)
