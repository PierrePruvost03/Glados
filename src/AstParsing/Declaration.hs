module AstParsing.Declaration where

import AstParsing.Expression (parseExpression)
import Parser
import DataStruct.Ast
import Control.Applicative
import AstParsing.Skip
import AstParsing.Type
import AstParsing.Utils

parseDeclaration :: Parser Ast
parseDeclaration = skip *> (
        AVarDecl <$>
            parseType <*>
            parseName <*>
            optional (parseChar '=' *> parseExpression)
    ) <* skip
