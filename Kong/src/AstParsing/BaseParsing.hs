module AstParsing.BaseParsing where

import AstParsing.Declaration
import AstParsing.Expression
import AstParsing.Include
import AstParsing.Keywords.Keywords
import AstParsing.Return
import AstParsing.Skip
import AstParsing.Struct
import AstParsing.Type
import AstParsing.Utils
import Control.Applicative
import DataStruct.Ast
import Parser

parseWhile :: Parser Ast
parseWhile =
  parseString symbolWhile
    *> ( ALoop Nothing
           <$> (parseChar symbolForIn *> (AExpress <$> parseExpression) <* parseChar symbolForOut)
           <*> pure Nothing
           <*> parseBody
       )

parseFor :: Parser Ast
parseFor =
  parseString symbolFor
    *> skip
    *> parseChar symbolForIn
    *> ( ALoop
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
    <$> (skip *> parseString symbolIf *> parseCond)
    <*> (skip *> parseBody)
    <*> optional (parseString symbolElse *> skip *> (parseBody <|> parseIf))

parseFunction :: Parser Ast
parseFunction =
  AFunkDef
    <$> (skip *> parseString symbolFunc *> skip *> parseName)
    <*> ( skip
            *> (parseChar symbolFuncParamIn <|> fatal "Function" ("missing char \"" <> [symbolFuncParamIn] <> "\""))
            *> parseMultiple parseDeclaration
            <* parseChar symbolFuncParamOut
        )
    <*> (skip *> parseString symbolFuncReturn *> parseType <* skip)
    <*> (parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut)

parseAstBlockContent :: Parser Ast
parseAstBlockContent =
  parseIf
    <|> parseFor
    <|> parseForIn
    <|> parseReturn
    <|> parseAstFile
    <|> AExpress <$> parseLineExpression

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
    <$> (skip *> parseChar symbolBlockIn *> parseAstBlock <* skip <* parseChar symbolBlockOut <* skip)

parseAst :: Parser [Ast]
parseAst = many parseAstFile
