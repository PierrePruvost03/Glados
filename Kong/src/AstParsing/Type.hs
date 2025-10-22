module AstParsing.Type (parseType, parseBaseType) where

import AstParsing.Expression (parseExpression)
import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils

parseBaseType :: Parser Type
parseBaseType = parseConstType f
    where f =
            skip *> (TKong <$> (parseString symbolUnsigned *> f) <|>
            TStrong <$> (parseString symbolLong *> f) <|>
            TInt <$ parseString symbolInt <|>
            TBool <$ parseString symbolBool <|>
            TChar <$ parseString symbolChar <|>
            TFloat <$ parseString symbolFloat <|>
            TTuple <$> (parseChar symbolTuple *>
                parseMultiple parseType <* parseChar symbolTuple) <|>
            TCustom <$> parseName) <* skip

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
                    (parseChar symbolVectorIn *> (parseExpression
                        <|> pure (AValue (ANumber (AInteger 0))))
                    <* parseChar symbolVectorOut) <|>
                TArray <$> (base <* skip) <*>
                    (parseChar symbolArrayIn *> skip *> parseExpression
                    <* skip <* parseChar symbolArrayOut)

parseRecParentType  :: Parser Type -> Parser Type
parseRecParentType  base = parseTry base *>
            skip *> (
                (base <* skip <* isNotWrapper) <|>
                parseRecParentType (parseParentType base)
            ) <* skip


parseType :: Parser Type
parseType = parseRecParentType parseBaseType
