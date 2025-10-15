module AstParsing.BaseParsing where

import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip

parseFunction :: Parser Ast
parseFunction = AFunkDef <$> parseString SYMBOL_FUNC *> skip *>
    parseName <*>

parseAst :: Parser Ast
parseAst = many $ skipChar " \n\t" *>
    parseFunction <|>
    parseDelcaration
    <* skip
