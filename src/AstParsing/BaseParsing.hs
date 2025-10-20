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
    (skip *> (parseChar symbolFuncParamIn <|> fatal "Function" ("missing char \"" <> [symbolFuncParamIn] <> "\"")) *>
        parseMultiple parseDeclaration <* parseChar symbolFuncParamOut) <*>
    (skip *> parseString symbolFuncReturn *> parseType <* skip) <*>
    (parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut)


parseAstBlock :: Parser [Ast]
parseAstBlock = many $ skip *>
    (parseAstFile <|> (AExpress <$> parseLineExpression))
    <* skip

parseAstFile :: Parser Ast
parseAstFile = skip *>
    parseFunction <|>
    parseLineDeclaration
    <* skip

parseBody :: Parser Ast
parseBody =
  ABlock
    <$> (skip *> parseChar symbolBlockIn *> many (AExpress <$> parseLineExpression) <* skip <* parseChar symbolBlockOut <* skip)


parseAst :: Parser [Ast]
parseAst = many parseAstFile
