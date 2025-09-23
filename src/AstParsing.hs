module AstParsing (
    ) where

import Control.Applicative ((<|>))
import Data.Traversable (traverse)
import DataStruct.Ast
import DataStruct.SExpr

parseValue :: SExpr -> Maybe Ast
parseValue (SInt (_, i)) = Just $ AValue (AstInteger i)
parseValue _ = Nothing

parseSymbol :: SExpr -> Maybe Ast
parseSymbol (SSymbol (_, i)) = Just $ ASymbol i
parseSymbol _ = Nothing

parseDefine :: SExpr -> Maybe Ast
parseDefine (SList (_, [SSymbol (_,"define"), SSymbol (_, s), q])) =
  case parseAstFromSExpr q of
    Just (AList _) -> Nothing
    Just a         -> Just $ ADefine s a
    Nothing        -> Nothing
parseDefine _ = Nothing

parseArgs :: SExpr -> Maybe Ast
parseArgs (SList (_, s)) =
    Just . AList <$> traverse parseSymbol s
parseArgs _ = Nothing

parseLambda :: SExpr -> Maybe Ast
parseLambda (SList (_, [SSymbol (_,"lambda"), s, q])) =
    ((Just . ALambdas . AstLambda) . AList <$> parseArgs s) <*> parseAstFromSExpr q
parseLambda _ = Nothing

parseCall :: SExpr -> Maybe Ast
parseCall (SList (_, [SSymbol (_, call), SList (_, arg)])) = Just . ACall call <$> parseArgs args
parseCall _ = Nothing

parseIf :: SExpr -> Maybe Ast
parseIf (SList (_, [SSymbol (_, "if"), cond, true, false])) =
    AIf <$> parseAstFromSExpr cond <*> parseAstFromSExpr true <*> parseAstFromSExpr false
parseIf _ = Nothing

parseList :: SExpr -> Maybe Ast
parseList (SList (_, xs)) = Just . AList <$> traverse parseAstFromSExpr xs
parseList _ = Nothing

parseAstFromSExpr :: SExpr -> Maybe Ast
parseAstFromSExpr sexpr =
     parseDefine sexpr
 <|> parseLambda sexpr
 <|> parseValue  sexpr
 <|> parseSymbol sexpr
 <|> parseIf sexpr
 <|> parseCall sexpr
 <|> parseList sexpr