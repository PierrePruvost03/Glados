module SExprParser (
    parseLisp,
    parseLispInt,
    parseLispSymbol,
    parseLispList,
) where

import Parser
import DataStruct.SExpr

parseLispList :: Parser SExpr
parseLispList = SList <$> ((,) <$> getLineCount <*> (parseChar '(' *> many parseLisp <* parseChar ')'))

parseLispInt :: Parser SExpr
parseLispInt = SInt <$> ((,) <$> getLineCount <*> parseInt)

parseLispSymbol :: Parser SExpr
parseLispSymbol = SSymbol <$> ((,) <$> getLineCount <*> some (parseAnyNotChar " \n()"))

parseLisp :: Parser SExpr
parseLisp = skipChars " \n\t" *>
    (
        parseLispList <|>
        parseLispInt <|>
        parseLispSymbol <|>
        generateError "SExpr parsing" "unknow syntax"
    ) <* skipChars " \n\t"
