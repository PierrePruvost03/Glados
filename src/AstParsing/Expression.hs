module AstParsing.Expression (parseExpression) where

import Parser
import DataStruct.Ast
import Parser
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
            ((parseString symbolTrue *> pure True) <|> (parseString symbolFalse *> pure False))
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
