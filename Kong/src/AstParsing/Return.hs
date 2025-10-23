module AstParsing.Return (parseReturn) where

import Parser
import DataStruct.Ast
import AstParsing.Expression (parseLineExpression)
import AstParsing.Keywords.Keywords

parseReturn :: Parser Ast
parseReturn = parseString symbolReturn *>
    (AReturn . AExpress <$> parseLineExpression)