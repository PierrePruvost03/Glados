module AstParsing.Struct (parseStruct) where

import Parser
import DataStruct.Ast
import AstParsing.Type
import AstParsing.Utils
import AstParsing.Skip
import AstParsing.Keywords.Keywords


parseField :: Parser (Type, String)
parseField = (,) <$> parseType <*> (skip *> (parseName <|> fatal "Struct" "missing field's name"))

parseStruct :: Parser Ast
parseStruct = AStruktDef
    <$> (parseString symbolStruct *> (parseName <|> fatal "Struct" "missing struct name"))
    <*> (skip *> (parseChar symbolStructIn <|> fatal "Struct" ("missing char \"" <> [symbolStructIn] <> "\""))
    *> many (skip *> parseField <* skip <* (parseChar symbolStructSep <|> fatal "Struct" ("missing char \"" <> [symbolStructSep] <> "\""))<* skip)
    <* parseChar symbolStructOut
    )