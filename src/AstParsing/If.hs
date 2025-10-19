module AstParsing.If () where

import AstParsing.BaseParsing
import AstParsing.Expression
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import DataStruct.Ast
import Parser

parseBody :: Parser Ast
parseBody =
  ABlock
    <$> (skip *> parseChar '{' *> parseAstBlock <* skip <* parseChar '}' <* skip)

parseCond :: Parser Ast
parseCond =
  skip *> parseChar '(' *> skip *> (AExpress <$> parseExpression) <* skip <* parseChar ')' <* skip

parseIf :: Parser Ast
parseIf =
  AIf
    <$> (parseString "if" *> parseCond)
    <*> (skip *> parseBody)
    <*> many ((,) <$> (skip *> parseString "elif" *> parseCond) <*> parseBody)
    <*> ((parseString "else" *> (Just <$> parseBody)) <|> pure Nothing)