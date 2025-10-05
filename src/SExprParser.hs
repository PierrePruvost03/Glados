module SExprParser
  ( parseLisp,
    parseLispInt,
    parseLispSymbol,
    parseLispList,
  )
where

import DataStruct.SExpr
import Parser

parseLispList :: Parser SExpr
parseLispList = SList <$> ((,) <$> getLineCount <*> (parseChar '(' *> many parseLisp <* parseChar ')'))

parseLispInt :: Parser SExpr
parseLispInt = SInt <$> ((,) <$> getLineCount <*> parseInt)

parseLispSymbol :: Parser SExpr
parseLispSymbol = SSymbol <$> ((,) <$> getLineCount <*> some (parseAnyNotChar "; \t\n()"))

parseComment :: Parser String
parseComment = (: []) <$> parseChar ';' <* parseUntilChar '\n'

parseLisp :: Parser SExpr
parseLisp =
  many (skipSomeChars " \n\t" <|> parseComment)
    *> skipChars " \n\t"
    *> ( parseLispList
           <|> parseLispInt
           <|> parseLispSymbol
           <|> (parseNotEmpty <|> generateError "nothing" "")
           *> generateError "SExpr parsing" "unknow syntax"
       )
    <* many (skipSomeChars " \n\t" <|> parseComment)
    <* skipChars " \n\t"
