module AstParsing.Declaration where

import AstParsing.Expression (parseExpression)
import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Type
import AstParsing.Utils

parseDeclaration :: Parser Ast
parseDeclaration = skip *> (
        AVarDecl <$>
            parseType <*>
            parseName <*>
            (((Just <$> (parseChar '=' *> parseExpression)) <|> pure Nothing))
    ) <* skip
