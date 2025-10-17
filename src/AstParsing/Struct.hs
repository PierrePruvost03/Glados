module AstParsing.Struct (parseStruct) where

import Parser
import DataStruct.Ast
import AstParsing.Type
import AstParsing.Utils
import AstParsing.Skip
import AstParsing.Keywords.Keywords


parseField :: Parser (Type, String)
parseField = (,) <$> parseType <*> (skip *> parseName)

parseStruct :: Parser Ast
parseStruct = AStruktDef
    <$> (parseString symbolStruct *> parseName)
    <*> (skip *> parseString symbolStructIn
    *> many (skip *> parseField <* skip <* parseChar ';' <* skip)
    <* parseString symbolStructOut
    )