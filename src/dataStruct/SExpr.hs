module Ast where

data SExpr = SInt Int
            | SSymbol String
            | SList [SExpr]
            deriving (Eq, Ord, Show)
