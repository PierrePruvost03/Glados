module AstParsing.If (parseIf) where

import AstParsing.BaseParsing
import AstParsing.Expression
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import DataStruct.Ast
import Parser

parseBody :: Parser Ast
parseBody =
  ABlock
    <$> (skip *> parseChar symbolAstBlockIn *> parseAstBlock <* skip <* parseChar symbolAstBlockOut <* skip)

parseCond :: Parser Ast
parseCond =
  skip *> parseChar symbolCondIn *> skip *> (AExpress <$> parseExpression) <* skip <* parseChar symbolCondOut <* skip

parseIf :: Parser Ast
parseIf =
  AIf
    <$> (parseString symbolIf *> parseCond)
    <*> (skip *> parseBody)
    <*> many ((,) <$> (skip *> parseString symbolElif *> parseCond) <*> parseBody)
    <*> ((parseString symbolElse *> (Just <$> parseBody)) <|> pure Nothing)