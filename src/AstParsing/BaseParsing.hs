module AstParsing.BaseParsing where

import AstParsing.Declaration
import AstParsing.Expression
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Type
import AstParsing.Utils
import AstParsing.Return
import AstParsing.Include
import AstParsing.Struct
import Control.Applicative
import DataStruct.Ast
import Parser

parseFor :: Parser Ast
parseFor =
  parseString symbolFor
    *> skip
    *> parseChar symbolForIn
    *> ( AFor
           <$> optional parseDeclaration
           <*> (parseChar symbolForSep *> (AExpress <$> parseExpression))
           <*> (parseChar symbolForSep *> optional (AExpress <$> parseExpression))
           <*> (parseChar symbolForOut *> skip *> parseBody)
       )

parseForIn :: Parser Ast
parseForIn =
  parseString symbolFor
    *> skip
    *> ( AForIn
           <$> parseName
           <* skip
           <* parseString symbolIn
           <*> (AExpress <$> parseExpression)
           <*> parseBody
       )

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

parseFunction :: Parser Ast
parseFunction = AFunkDef <$>
    (skip *> parseString symbolFunc *> skip *> parseName) <*>
    (skip *> (parseChar symbolFuncParamIn <|> fatal "Function" ("missing char \"" <> [symbolFuncParamIn] <> "\"")) *>
        parseMultiple parseDeclaration <* parseChar symbolFuncParamOut) <*>
    (skip *> parseString symbolFuncReturn *> parseType <* skip) <*>
    (parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut)

parseAstBlockContent :: Parser Ast
parseAstBlockContent =
    parseIf
    <|> parseFor
    <|> parseForIn
    <|> parseReturn
    <|> parseAstFile
    <|> (AExpress <$> parseLineExpression)

parseAstBlock :: Parser [Ast]
parseAstBlock =
  many $
    skip
      *> parseAstBlockContent
      <* skip

parseAstFile :: Parser Ast
parseAstFile =
  skip
    *> parseFunction
    <|> parseInclude
    <|> parseLineDeclaration
    <|> parseStruct
      <* skip

parseBody :: Parser Ast
parseBody =
  ABlock
    <$> (skip *> parseChar symbolBlockIn *> many (AExpress <$> parseLineExpression) <* skip <* parseChar symbolBlockOut <* skip)

parseAst :: Parser [Ast]
parseAst = many parseAstFile
