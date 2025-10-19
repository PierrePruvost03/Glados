module AstParsing.For () where

import AstParsing.BaseParsing
import AstParsing.Declaration
import AstParsing.Expression
import AstParsing.Keywords.Keywords
import AstParsing.Skip
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
           <*> (parseChar symbolForSep *> optional (AExpress <$> parseExpression))
           <*> (parseChar symbolForSep *> optional (AExpress <$> parseExpression))
           <*> (parseChar symbolForOut *> skip *> parseChar symbolBlockIn *> (ABlock <$> parseAstBlock) <* parseChar symbolBlockOut)
       )