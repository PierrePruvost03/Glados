module AstParsing.BaseParsing (parseAst) where

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
           <*> ((parseChar symbolForSep *> (AExpress <$> parseExpression)) <|> fatal "For" "invalid condition")
           <*> (parseChar symbolForSep *> optional (AExpress <$> parseExpression))
           <*> ((parseChar symbolForOut <|> fatal "For" ("missing char \"" <> [symbolForOut] <> "\"")) *> skip *> (parseBody <|> fatal "For" "invalid body"))
       )

parseForIn :: Parser Ast
parseForIn =
  parseString symbolFor
    *> skip
    *> ( AForIn
           <$> (parseName <|> fatal "For In" "missing variable")
           <* skip
           <* parseString symbolIn
           <*> ((AExpress <$> parseExpression) <|> fatal "For In" "invalid expression")
           <*> (parseBody <|> fatal "For In" "invalid body")
       )

parseCond :: Parser Ast
parseCond =
  skip *> parseChar symbolCondIn *> skip *> (AExpress <$> parseExpression) <* skip <* parseChar symbolCondOut <* skip

parseIf :: Parser Ast
parseIf =
  AIf
    <$> (skip *> parseString symbolIf *> (parseCond <|> fatal "If" "invalid condition"))
    <*> ((skip *> parseBody) <|> fatal "If" "invalid code block")
    <*> optional (parseString symbolElse *> skip *> (parseBody <|> parseIf <|> fatal "Else" "invalid else body"))

parseFunction :: Parser Ast
parseFunction =
  AFunkDef
    <$> (skip *> parseString symbolFunc *> skip *> parseName)
    <*> ( skip
            *> (parseChar symbolFuncParamIn <|> fatal "Function" ("missing char \"" <> [symbolFuncParamIn] <> "\""))
            *> parseMultiple parseDeclaration
            <* (parseChar symbolFuncParamOut <|> fatal "Function" ("missing char \"" <> [symbolFuncParamOut] <> "\""))
        )
    <*> ((skip *> parseString symbolFuncReturn *> parseType <* skip) <|> fatal "Function" ("missing return value type after \"" <> symbolFuncReturn <> " symbol\""))
    <*> ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Function" "invalid body")

parseAstBlockContent :: Parser Ast
parseAstBlockContent =
  parseIf
    <|> parseWhile
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
