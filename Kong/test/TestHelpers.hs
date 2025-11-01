module TestHelpers
  ( lc
  , wrapAst
  , wrapExpr
  , wrapValue
  , wrapType
  , wrapAccess
  ) where

import DataStruct.Ast
import Parser (LineCount)

lc :: LineCount
lc = (0, 0)

wrapAst :: AstRaw -> Ast
wrapAst = (,) lc

wrapExpr :: AExpressionRaw -> AExpression
wrapExpr = (,) lc

wrapValue :: AstValueRaw -> AstValue
wrapValue = (,) lc

wrapType :: TypeRaw -> Type
wrapType = (,) lc

wrapAccess :: AstAccessRaw -> AstAccess
wrapAccess = (,) lc
