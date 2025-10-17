module AstParsing.Expression (parseExpression) where

import Parser
import DataStruct.Ast
import Data.Functor
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils
import Control.Applicative
import Data.Maybe

parseStringValue :: Parser AstValue
parseStringValue = AString <$> parseBetween '"'

parseNumberValue :: Parser AstValue
parseNumberValue = ANumber <$>
    (parseAstFloat <|> parseAstInt <|> parseAstBool <|> parseAstChar)
    where
        parseAstFloat = AFloat <$> parseFloat
        parseAstInt = AInteger <$> parseInt
        parseAstBool = ABool <$>
            (parseString symbolTrue $> True <|> parseString symbolFalse $> False)
        parseAstChar = AChar <$> (parseChar '\'' *> parseCharAny <* parseChar '\'')

parseWrapper :: Char -> Char -> Parser [AExpression]
parseWrapper i o =
    parseChar i *>
    parseMultiple parseExpression <*
    parseChar o

parseTupleValue :: Parser AstValue
parseTupleValue = ATuple <$> parseWrapper symbolTuple symbolTuple

parseArrayValue :: Parser AstValue
parseArrayValue = AArray <$> parseWrapper symbolArrayIn symbolArrayOut

parseVectorValue :: Parser AstValue
parseVectorValue = AVector <$> parseWrapper symbolVectorIn symbolVectorOut

parseStructValue :: Parser AstValue
parseStructValue = AStruct <$> (
    parseChar symbolStructIn *>
    parseMultiple ((,) <$>
        parseName <* skip <* parseChar '=' <* skip <*> parseExpression)
    <* parseChar symbolStructOut
    )

parseVarCall :: Parser AstValue
parseVarCall = AVarCall <$> parseName

parseValue :: Parser AExpression
parseValue = skip *> (AValue <$> (
        parseStringValue <|>
        parseNumberValue <|>
        parseTupleValue <|>
        parseArrayValue <|>
        parseVectorValue <|>
        parseStructValue <|>
        parseVarCall
    )) <* skip

parseInfix :: Parser AExpression
parseInfix = skip *> (
        ((\e1 n e2 -> ACall n (catMaybes [e1, Just e2])) <$>
        (optional parseBasicExpression) <* skip <*>
        (parseBetween '`' <|> ((:[]) <$> parseAnyChar allowedInfix)) <* skip <*>
        parseExpression))

parseCall :: Parser AExpression
parseCall = skip *> (
        (ACall <$>
            parseName <* skip <*>
            (parseChar '(' *> parseMultiple parseExpression <* parseChar ')'))
    ) <* skip


parseWrapperAccess :: (String -> AExpression -> AstAccess) -> Char -> Char -> Parser AstAccess
parseWrapperAccess f i o = f <$> (parseName <* skip) <*> (parseChar i *> parseExpression <* parseChar o)

parseAccess :: Parser AExpression
parseAccess = AAccess <$> (skip *> (
    parseWrapperAccess AArrayAccess symbolArrayIn symbolArrayOut <|>
    parseWrapperAccess AVectorAccess symbolVectorIn symbolVectorOut <|>
    parseWrapperAccess ATupleAccess symbolTuple symbolTuple <|>
    (AStructAccess <$>
            (parseName <* skip) <*>
            (parseChar symbolStructIn *>
                parseMultiple (skip *> parseName <* skip)
                <* skip <* parseChar symbolStructOut))
    ))

parseBasicExpression :: Parser AExpression
parseBasicExpression =  parseAccess <|> parseCall <|> parseValue

parseExpression :: Parser AExpression
parseExpression = (parseInfix <|> parseBasicExpression) *> parseChar ';'
