module AstParsing.If (parseIf) where

import AstParsing.BaseParsing
import AstParsing.Expression
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import DataStruct.Ast
import Parser

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