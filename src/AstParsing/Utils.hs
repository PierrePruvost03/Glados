module AstParsing.Utils where

import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip

parseMultiple :: Parser a -> Parser [a]
parseMultiple p = parseMultipleSep p ','

parseMultipleSep :: Parser a -> Char -> Parser [a]
parseMultipleSep p s = ((:) <$> p <*> many (skip *> parseChar s *> p)) <|> pure []


parseName :: Parser String
parseName = skip *>
    parseSomeUntilAnyNotChar
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    <* skip
