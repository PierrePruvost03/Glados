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
parseLispSymbol = SSymbol <$> ((,) <$> getLineCount <*> some (parseAnyNotChar "; \n()"))

parseComment :: Parser String
parseComment = (:[]) <$> parseChar ';' <* parseUntilChar '\n'

parseLisp :: Parser SExpr
parseLisp = many (skipSomeChars " \n\t" <|> parseComment) *>
    (
        parseLispList <|>
        parseLispInt <|>
        parseLispSymbol <|>
        generateError "SExpr parsing" "unknow syntax"
    ) <* many (skipSomeChars " \n\t" <|> parseComment)
