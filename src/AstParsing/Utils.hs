module AstParsing.Utils where

import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip

parseMultiple :: Parser a -> Parser [a]
parseMultiple p = ((:) <$> p <*> many (skip *> parseChar ',' *> p)) <|> pure []

parseName :: Parser String
parseName = skip *>
    parseSomeUntilAnyNotChar
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    <* skip
