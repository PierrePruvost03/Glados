module AstParsing.BaseParsing where

import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Keywords.Keywords
import AstParsing.Utils
import AstParsing.Type
import AstParsing.Declaration
import AstParsing.Expression

parseFunction :: Parser Ast
parseFunction = AFunkDef <$>
    (skip *> parseString symbolFunc *> skip *> parseName) <*>
    (skip *> parseChar '(' *> parseMultiple parseDeclaration <* parseChar ')') <*>
    (skip *> parseString "->" *> parseType <* skip) <*>
    (parseChar '{' *> ((:[]) <$> (AExpress <$> parseExpression)) <* parseChar '}')

parseAst :: Parser [Ast]
parseAst = many $ skip *>
    parseFunction <|>
    parseDeclaration
    <* skip
