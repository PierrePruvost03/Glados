module AstParsing.Expression (parseExpression) where

import Parser
import DataStruct.Ast

parseExpression :: Parser AExpression
parseExpression = Parser $ \r -> Right (AValue (ANumber (AInteger 0)), r)
