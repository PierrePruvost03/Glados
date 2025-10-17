module AstParsing.Utils
  ( parseMultiple,
    parseName,
  )
where

import AstParsing.Skip
import Parser

parseMultiple :: Parser a -> Parser [a]
parseMultiple p = ((:) <$> p <*> many (skip *> parseChar ',' *> p)) <|> pure []

parseName :: Parser String
parseName =
  skip
    *> parseSomeUntilAnyNotChar
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    <* skip
