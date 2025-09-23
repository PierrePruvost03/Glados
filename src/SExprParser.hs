module SExprParser (
    SExpr (SInt, SSymbol, SList),
    parseLisp,
    parseLispInt,
    parseLispSymbol,
    parseLispList,
) where

import Parser

data SExpr = SInt (LineCount, Int)
            | SSymbol (LineCount, String)
            | SList (LineCount, [SExpr])
        deriving (Eq, Ord, Show)

parseLispList :: Parser SExpr
parseLispList = SList <$> ((,) <$> getLineCount <*> (parseChar '(' *> many parseLisp <* parseChar ')'))

parseLispInt :: Parser SExpr
parseLispInt = SInt <$> ((,) <$> getLineCount <*> parseInt)

parseLispSymbol :: Parser SExpr
parseLispSymbol = SSymbol <$> ((,) <$> getLineCount <*> (some $ parseAnyNotChar " \n()"))

parseLisp :: Parser SExpr
parseLisp = skipChars " \n\t" *>
    (
        parseLispList <|>
        parseLispInt <|>
        parseLispSymbol <|>
        generateError "SExpr parsing" "unknow syntax"
    ) <* skipChars " \n\t"
