module AstParsing.Declaration where

import AstParsing.Expression (parseExpression)
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Type
import AstParsing.Utils
import Control.Applicative
import DataStruct.Ast
import Parser

parseDeclaration :: Parser Ast
parseDeclaration =
  skip
    *> ( AVarDecl
           <$> parseType
           <*> parseName
           <*> optional (parseChar symbolDeclaration *> parseExpression)
       )
    <* skip

parseLineDeclaration :: Parser Ast
parseLineDeclaration = parseDeclaration <* parseChar symbolEndOfDeclaration
