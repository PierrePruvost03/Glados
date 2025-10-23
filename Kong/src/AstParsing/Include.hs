module AstParsing.Include where

import AstParsing.Skip
import Parser
import DataStruct.Ast
import AstParsing.Utils
import AstParsing.Keywords.Keywords

parseInclude :: Parser Ast
parseInclude = AInclude <$>
    (skip *> parseString symbolInclude  *>
        parseName)
        <*> ((skip *> parseString symbolIncludeIn
            *> parseManyWithSeparator (skip *> parseName <* skip) symbolIncludeSep
             <* parseString symbolIncludeOut) <|> pure [])