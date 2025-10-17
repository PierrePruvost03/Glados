module AstParsing.BaseParsing where

import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils
import AstParsing.Type
import AstParsing.Declaration
import AstParsing.Expression (parseLineExpression)

parseFunction :: Parser Ast
parseFunction = AFunkDef <$>
    (skip *> parseString symbolFunc *> skip *> parseName) <*>
    (skip *> parseChar '(' *> parseMultiple parseDeclaration <* parseChar ')') <*>
    (skip *> parseString "->" *> parseType <* skip) <*>
    (parseChar '{' *> parseAstBlock <* parseChar '}')

parseAstBlock :: Parser [Ast]
parseAstBlock = many $ skip *>
    (parseAstFile <|> (AExpress <$> parseLineExpression))
    <* skip

parseAstFile :: Parser Ast
parseAstFile = skip *>
    parseFunction <|>
    parseLineDeclaration
    <* skip

parseAst :: Parser [Ast]
parseAst = many $ parseAstFile
