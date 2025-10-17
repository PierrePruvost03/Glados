module AstParsing.Expression (parseExpression) where

import Parser
import DataStruct.Ast
import Parser
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils

parseStringValue :: Parser AstValue
parseStringValue = AString <$> parseBetween '"'

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

parseVarCall :: Parser AstValue
parseVarCall = AVarCall <$> parseName

parseValue :: Parser AExpression
parseValue = skip *> (AValue <$> (
        parseStringValue <|>
        parseNumberValue <|>
        parseTupleValue <|>
        parseVarCall
    )) <* skip

parseCall :: Parser AExpression
parseCall = skip *> (
        ((\e1 n e2 -> ACall n [e1, e2]) <$>
        parseExpression <* skip <*>
        (parseBetween '`' <|> ((:[]) <$> parseCharAny)) <* skip <*>
        parseExpression) <|>
        (ACall <$> parseName <* skip <*> (parseChar '(' *> parseMultiple parseExpression <* parseChar ')'))
    ) <* skip

parseExpression :: Parser AExpression
parseExpression = parseValue
