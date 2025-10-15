module AstParsing.Type (parseType) where

import AstParsing.Expression (parseExpression)
import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip

parseBaseType :: Parser Type
parseBaseType = parseConstType f
    where f =
            skip *> (TKong <$> (parseString symbolUnsigned *> parseBaseType) <|>
            TStrong <$> (parseString symbolLong *> parseBaseType) <|>
            TInt <$ parseString symbolInt <|>
            TBool <$ parseString symbolBool <|>
            TChar <$ parseString symbolChar <|>
            TFloat <$ parseString symbolFloat) <* skip

parseConstType :: Parser Type -> Parser Type
parseConstType base = skip *>
    (TKonst <$> (base <* skip <* parseString symbolConst) <|>
    base) <* skip

isNotWrapper :: Parser ()
isNotWrapper = isAnyNotChar wrapperList

parseParentType :: Parser Type -> Parser Type
parseParentType base = parseConstType (skip *> f <* skip)
        where f =
                TVector <$> (base <* skip) <*>
                    (parseChar symbolVecIn *> parseExpression
                    <* parseChar symbolVecOut) <|>
                TArray <$> (base <* skip) <*>
                    (parseChar symbolArrayIn *> skip *> parseInt
                    <* skip <* parseChar symbolArrayOut)

parseRecParentType  :: Parser Type -> Parser Type
parseRecParentType  base = parseTry base *>
            skip *> (
                (base <* skip <* isNotWrapper) <|>
                parseRecParentType (parseParentType base)
            ) <* skip


parseType :: Parser Type
parseType = parseRecParentType parseBaseType
