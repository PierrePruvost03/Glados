module AstParsing.Include
(
    parseInclude
) where

import AstParsing.Skip
import Parser
import DataStruct.Ast
import AstParsing.Utils
import AstParsing.Keywords.Keywords

parseInclude :: Parser Ast
parseInclude = wrap $ AInclude <$>
    (skip *> parseString symbolInclude  *>
        (parseIncludePath <|> fatal "Include" "missing include name"))
        <*> ((skip *> parseString symbolIncludeIn
            *> parseManyWithSeparator (skip *> parseName <* skip) symbolIncludeSep
             <* (parseString symbolIncludeOut <|> fatal "Include" ("missing char \"" <> symbolIncludeOut <> "\""))) <|> pure [])

parseIncludePath :: Parser String
parseIncludePath = skip *> parseSomeUntilAnyNotChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_/-." <* skip
