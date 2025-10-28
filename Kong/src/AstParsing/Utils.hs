{-# LANGUAGE NamedFieldPuns #-}
module AstParsing.Utils
  ( parseMultiple,
    parseName,
    getVarType
  )
where

import AstParsing.Skip
import DataStruct.Ast
import Parser

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

getVarType :: Ast -> Type
getVarType (AVarDecl {varType}) = varType
getVarType _ = TInt
