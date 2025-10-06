module AstParsing
  ( parseAstFromSExpr,
  )
where

import DataStruct.Ast
import DataStruct.SExpr
import Parser (LineCount)

type AstResult a = Either (String, String, LineCount) a

ok :: a -> AstResult a
ok = Right

notFoundAt :: LineCount -> AstResult a
notFoundAt lc = Left ("parse", "no such element found", lc)

malformed :: String -> String -> LineCount -> AstResult a
malformed ctx msg lc = Left (ctx, msg, lc)

(<?|>) :: AstResult a -> AstResult a -> AstResult a
(<?|>) (Left ("parse", "no such element found", _)) r = r
(<?|>) l _ = l

isQuotedString :: String -> Bool
isQuotedString ('"' : xs@(_ : _)) = case reverse xs of
  '"' : _ -> True
  _ -> False
isQuotedString _ = False

stripQuotes :: String -> String
stripQuotes ('"' : xs@(_ : _)) = case reverse xs of
  '"' : rest -> reverse rest
  _ -> '"' : xs
stripQuotes s = s

parseValue :: SExpr -> AstResult Ast
parseValue (SInt (lc, i)) = ok $ AValue (lc, AstInteger i)
parseValue (SSymbol (lc, s))
  | isQuotedString s = ok $ AValue (lc, AstString (stripQuotes s))
  | otherwise = notFoundAt lc
parseValue (SBool (lc, b)) = ok $ AValue (lc, AstBool b)
parseValue (SList (lc, _)) = notFoundAt lc

parseSymbol :: SExpr -> AstResult Ast
parseSymbol (SSymbol (lc, i)) = ok $ ASymbol (lc, i)
parseSymbol (SInt (lc, _)) = notFoundAt lc
parseSymbol (SList (lc, _)) = notFoundAt lc
parseSymbol (SBool (lc, _)) = notFoundAt lc

parseDefine :: SExpr -> AstResult Ast
parseDefine (SList (lc, [SSymbol (_, "define"), SSymbol (lcName, s), q])) =
  case parseAstFromSExpr q of
    Right a -> ok $ ADefine {name = (lcName, s), value = (lc, a)}
    Left e@(ctx, _, _) | ctx /= "parse" -> Left e
    Left _ -> malformed "define" "malformed value expression" lc
parseDefine (SList (lc, SSymbol (_, "define") : _)) =
  malformed "define" "expected (define <name> <expr>)" lc
parseDefine (SList (lc, _)) = notFoundAt lc
parseDefine (SInt (lc, _)) = notFoundAt lc
parseDefine (SSymbol (lc, _)) = notFoundAt lc
parseDefine (SBool (lc, _)) = notFoundAt lc

parseArgs :: SExpr -> AstResult Ast
parseArgs (SList (lc, s)) = foldl step (ok (AList (lc, []))) s
  where
    step acc x = case (acc, parseSymbol x) of
      (Right (AList (lcA, xs)), Right (ASymbol sym)) -> ok $ AList (lcA, xs ++ [ASymbol sym])
      (Right (AList _), Right _) -> malformed "lambda-args" "arguments must be symbols" lc
      (Left e, _) -> Left e
      (_, Left e@(ctx, _, _)) | ctx /= "parse" -> Left e
      _ -> malformed "lambda-args" "arguments must be symbols" lc
parseArgs (SSymbol (lc, _)) = malformed "lambda-args" "expected list of symbols, got symbol" lc
parseArgs (SInt (lc, _)) = malformed "lambda-args" "expected list of symbols, got integer" lc
parseArgs (SBool (lc, _)) = malformed "lambda-args" "expected list of symbols, got boolean" lc

parseLambda :: SExpr -> AstResult Ast
parseLambda (SList (lc, [SSymbol (_, "lambda"), s, q])) = case (parseArgs s, parseAstFromSExpr q) of
  (Right (AList (_, arg)), Right body) -> ok $ ALambdas (lc, AstLambda arg body)
  (Left e@(ctx, _, _), _) | ctx /= "parse" -> Left e
  (_, Left e@(ctx, _, _)) | ctx /= "parse" -> Left e
  _ -> malformed "lambda" "malformed lambda body or arguments" lc
parseLambda (SList (lc, SSymbol (_, "lambda") : _)) =
  malformed "lambda" "expected (lambda (<args>) <body>)" lc
parseLambda (SList (lc, _)) = notFoundAt lc
parseLambda (SInt (lc, _)) = notFoundAt lc
parseLambda (SSymbol (lc, _)) = notFoundAt lc
parseLambda (SBool (lc, _)) = notFoundAt lc

parseCall :: SExpr -> AstResult Ast
parseCall (SList (lc, lambda@(SList (_, SSymbol (lcLam, "lambda") : _)) : arg)) = case parseLambda lambda of
  Right (ALambdas (_, result)) -> case parseList (SList (lc, arg)) of
    Right lst -> ok $ ACall {callRef = (lcLam, Right result), args = (lc, lst)}
    Left e@(ctx, _, _) | ctx /= "parse" -> Left e
    Left _ -> malformed "call" "malformed argument list" lc
  Right _ -> notFoundAt lc
  err@(Left _) -> err
parseCall (SList (lc, SSymbol (lcName, call) : arg)) =
  case parseList (SList (lc, arg)) of
    Right lst -> ok $ ACall {callRef = (lcName, Left call), args = (lc, lst)}
    Left e@(ctx, _, _) | ctx /= "parse" -> Left e
    Left _ -> malformed "call" "malformed argument list" lc
parseCall (SList (lc, _)) = notFoundAt lc
parseCall (SInt (lc, _)) = notFoundAt lc
parseCall (SSymbol (lc, _)) = notFoundAt lc
parseCall (SBool (lc, _)) = notFoundAt lc

parseIf :: SExpr -> AstResult Ast
parseIf (SList (lc, [SSymbol (_, "if"), cond, tExp, fExp])) =
  case (parseAstFromSExpr cond, parseAstFromSExpr tExp, parseAstFromSExpr fExp) of
    (Right c, Right t, Right f) -> ok $ AIf {ifCond = (lc, c), ifThen = (lc, t), ifElse = (lc, f)}
    (Left e@(ctx, _, _), _, _) | ctx /= "parse" -> Left e
    (_, Left e@(ctx, _, _), _) | ctx /= "parse" -> Left e
    (_, _, Left e@(ctx, _, _)) | ctx /= "parse" -> Left e
    _ -> malformed "if" "malformed condition or branches" lc
parseIf (SList (lc, SSymbol (_, "if") : _)) =
  malformed "if" "expected (if <cond> <then> <else>)" lc
parseIf (SList (lc, _)) = notFoundAt lc
parseIf (SInt (lc, _)) = notFoundAt lc
parseIf (SSymbol (lc, _)) = notFoundAt lc
parseIf (SBool (lc, _)) = notFoundAt lc

parseList :: SExpr -> AstResult Ast
parseList (SList (lc, xs)) = foldl cons (ok (AList (lc, []))) xs
  where
    cons acc x = case (acc, parseAstFromSExpr x) of
      (Right (AList (lcL, as)), Right a) -> ok $ AList (lcL, as ++ [a])
      (Left e, _) -> Left e
      (_, Left e@(ctx, _, _)) | ctx /= "parse" -> Left e
      _ -> malformed "list" "malformed element" lc
parseList (SInt (lc, _)) = notFoundAt lc
parseList (SSymbol (lc, _)) = notFoundAt lc
parseList (SBool (lc, _)) = notFoundAt lc

parseAstFromSExpr :: SExpr -> AstResult Ast
parseAstFromSExpr sexpr =
  parseDefine sexpr
    <?|> parseLambda sexpr
    <?|> parseValue sexpr
    <?|> parseSymbol sexpr
    <?|> parseIf sexpr
    <?|> parseCall sexpr
    <?|> parseList sexpr
