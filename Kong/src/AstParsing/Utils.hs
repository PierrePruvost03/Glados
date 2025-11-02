{-# LANGUAGE NamedFieldPuns #-}
module AstParsing.Utils
  ( parseMultiple,
    parseName,
    getVarType,
    wrap,
    (?:)
  )
where

import AstParsing.Skip
import DataStruct.Ast
import Parser

wrap :: Parser a -> Parser (Wrapper a)
wrap p = (,) <$> getLineCount <*> p

(?:) :: Bool -> (a, a) -> a
(?:) True = fst
(?:) _ = snd

parseMultiple :: Parser a -> Parser [a]
parseMultiple p = parseMultipleSep p ','

parseMultipleSep :: Parser a -> Char -> Parser [a]
parseMultipleSep p s = ((:) <$> p <*> many (skip *> parseChar s *> p)) <|> pure []


parseName :: Parser String
parseName =
  skip
    *> parseSomeUntilAnyNotChar
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    <* skip

getVarType :: AstRaw -> TypeRaw
getVarType (AVarDecl {varType}) = snd varType
getVarType _ = TInt
