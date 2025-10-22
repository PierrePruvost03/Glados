module AstParsing.Skip
  ( skip,
  )
where

import AstParsing.Keywords.Keywords
import Parser

skipUselessChars :: Parser String
skipUselessChars = skipSomeChars " \n\t"

skipLineComment :: Parser String
skipLineComment = parseString symbolLineComment *> parseUntilChar '\n'

skipComment :: Parser String
skipComment = parseString symbolCommentIn *> parseUntilString symbolCommentOut *> parseString symbolCommentOut

skip :: Parser [String]
skip = many (skipLineComment <|> skipComment <|> skipUselessChars)
