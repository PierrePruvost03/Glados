module AstParsing.Expression (parseExpression) where

import Parser
import DataStruct.Ast
import Data.Functor
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils

parseStringValue :: Parser AstValue
parseStringValue = AString <$> (parseChar '"' *> parseUntilChar '"' <* parseChar '"')

parseNumberValue :: Parser AstValue
parseNumberValue = ANumber <$>
    (parseAstFloat <|> parseAstInt <|> parseAstBool <|> parseAstChar)
    where
        parseAstFloat = AFloat <$> parseFloat
        parseAstInt = AInteger <$> parseInt
        parseAstBool = ABool <$>
            (parseString symbolTrue $> True <|> parseString symbolFalse $> False)
        parseAstChar = AChar <$> (parseChar '\'' *> parseCharAny <* parseChar '\'')

parseTupleValue :: Parser AstValue
parseTupleValue = ATuple <$>
    (parseChar symbolTuple *>
    parseMultiple parseExpression <*
    parseChar symbolTuple)

parseValue :: Parser AExpression
parseValue = skip *> (AValue <$> (
        parseStringValue <|>
        parseNumberValue <|>
        parseTupleValue
    )) <* skip

parseExpression :: Parser AExpression
parseExpression = parseValue
