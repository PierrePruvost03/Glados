{-# LANGUAGE NamedFieldPuns #-}
module AstParsing.BaseParsing (parseAst, parseType, parseExpression, parseDeclaration) where

import AstParsing.Include
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils
import Control.Applicative
import DataStruct.Ast
import Data.Functor
import Data.Maybe
import Control.Applicative
import Parser

------------------------------------------------
-- Type Declaration
------------------------------------------------
parseField :: Parser (Type, String)
parseField = (,) <$> parseType <*> (skip *> (parseName <|> fatal "Struct" "missing field's name"))

parseStruct :: Parser Ast
parseStruct = AStruktDef
    <$> (parseString symbolStruct *> (parseName <|> fatal "Struct" "missing struct name"))
    <*> (skip *> (parseChar symbolStructIn <|> fatal "Struct" ("missing char \"" <> [symbolStructIn] <> "\""))
    *> many (skip *> parseField <* skip <* (parseChar symbolStructSep <|> fatal "Struct" ("missing char \"" <> [symbolStructSep] <> "\""))<* skip)
    <* parseChar symbolStructOut
    )

parseTraitBody :: Parser [(String, [Type], Type)]
parseTraitBody = (
    (parseChar symbolBlockIn <|> fatal "Trait" ("missing char \"" <> [symbolBlockIn] <> "\"")) *> skip *>
    many (
        (,,) <$>
            (skip *> (parseName) <* skip) <*>
            (parseChar symbolFuncParamIn *> parseMultiple parseType <* skip <* parseChar symbolFuncParamOut) <*>
            (skip *> parseString symbolFuncReturn *> skip *> parseType <* skip <* parseChar symbolStructSep) <* skip) <*
    (parseChar symbolBlockOut <|> fatal "Trait" ("missing char \"" <> [symbolBlockOut] <> "\"")))

parseTrait :: Parser Ast
parseTrait = skip *> (
        ATraitDef <$>
            (parseString symbolTrait *> skip *> (parseName <|> fatal "Trait" "invalid trait name") <* skip) <*>
            (parseTraitBody)
    )

parseTraitFuncDef :: Type -> Parser Ast
parseTraitFuncDef t = f <$> ((,,,)
    <$> (skip *> parseName)
    <*> (skip
            *> (parseChar symbolFuncParamIn <|> fatal "Method" ("missing char \"" <> [symbolFuncParamIn] <> "\""))
            *> skip *> (parseString symbolSelf <|> fatal "Method" "missing self declaration")
                *> skip *> parseChar ',' *> parseMultiple (parseTraitDeclaration t)
            <* (parseChar symbolFuncParamOut <|> fatal "Method" ("missing char \"" <> [symbolFuncParamOut] <> "\""))
        )
    <*> ((skip *> parseString symbolFuncReturn *> parseTraitType t <* skip) <|> fatal "Method" ("missing return value type after \"" <> symbolFuncReturn <> " symbol\""))
    <*> ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Method" "invalid body"))
    where
        f (name, args, ret, body) = AVarDecl
            (TKonst (TFunc (t:(map getVarType args)) ret))
            ((show t) <> ('$':name))
            (Just (AValue (ALambda (AVarDecl t symbolSelf Nothing : args) ret body)))

parseTraitImpl :: Parser Ast
parseTraitImpl = skip *> (
    (,) <$> (parseString symbolImpl *> skip *> parseName <* skip) <*> (parseType <* skip) >>=
        \(n, t) -> ATraitImpl <$> pure n <*> pure t <*>
            ((parseChar symbolBlockIn <|> fatal "Trait" ("missing char \"" <> [symbolBlockIn] <> "\"")) *> skip *>
            many (skip *> parseTraitFuncDef t) <* skip <*
            (parseChar symbolBlockOut <|> fatal "Trait" ("missing char \"" <> [symbolBlockOut] <> "\"")))
        ) <* skip


------------------------------------------------
-- Declaration
------------------------------------------------
parseTraitDeclaration :: Type -> Parser Ast
parseTraitDeclaration t =
  skip
    *> ( AVarDecl
           <$> parseTraitType t
           <*> parseName
           <*> optional (parseChar symbolDeclaration *> parseExpression)
       )
    <* skip

parseDeclaration :: Parser Ast
parseDeclaration =
  skip
    *> ( AVarDecl
           <$> parseType
           <*> parseName
           <*> optional (parseChar symbolDeclaration *> parseExpression)
       )
    <* skip

parseLineDeclaration :: Parser Ast
parseLineDeclaration = parseDeclaration <* parseChar symbolEndOfDeclaration


------------------------------------------------
-- Types
------------------------------------------------
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
            TCustom <$> parseName <|>
            TFunc <$>
                (parseChar symbolFuncParamIn *> parseMultiple parseType <* (parseChar symbolFuncParamOut <|>
                    fatal "Function" ("missing char \"" <> [symbolFuncParamOut] <> "\"")) <* skip) <*>
                (parseString symbolFuncReturn *> skip *> parseType))
            <* skip


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

parseTraitType :: Type -> Parser Type
parseTraitType t = parseRecParentType $ (skip *> parseString symbolSelfType *> pure t) <|> parseBaseType

parseType :: Parser Type
parseType = parseRecParentType parseBaseType


------------------------------------------------
-- Expressions
------------------------------------------------
parseStringValue :: Parser AstValue
parseStringValue = AString <$> parseBetween symbolStringDelimiter

parseNumberValue :: Parser AstValue
parseNumberValue =
  ANumber
    <$> (parseAstInt <|> parseAstFloat <|> parseAstBool <|> parseAstChar)
  where
    parseAstFloat = AFloat <$> parseFloat
    parseAstInt = AInteger <$> parseInt
    parseAstBool =
      ABool
        <$> (parseString symbolTrue $> True <|> parseString symbolFalse $> False)
    parseAstChar = AChar <$> (parseChar symbolCharDelimiter *> parseCharAny <* parseChar symbolCharDelimiter)

parseWrapper :: Char -> Char -> Parser [AExpression]
parseWrapper i o =
  parseChar i
    *> parseMultiple parseExpression
    <* parseChar o

parseTupleValue :: Parser AstValue
parseTupleValue = ATuple <$> parseWrapper symbolTuple symbolTuple

parseArrayValue :: Parser AstValue
parseArrayValue = AArray <$> parseWrapper symbolArrayIn symbolArrayOut

parseVectorValue :: Parser AstValue
parseVectorValue = AVector <$> parseWrapper symbolVectorIn symbolVectorOut

parseStructValue :: Parser AstValue
parseStructValue =
  AStruct
    <$> ( parseChar symbolStructIn
            *> parseMultiple
              ( (,)
                  <$> parseName
                  <* skip
                  <* parseChar symbolDeclaration
                  <* skip
                  <*> parseExpression
              )
            <* parseChar symbolStructOut
        )

parseVarCall :: Parser AstValue
parseVarCall = AVarCall <$> parseName

parseLambda :: Parser AstValue
parseLambda = ALambda <$>
    (parseChar symbolFuncParamIn *> parseMultiple parseDeclaration <* parseChar symbolFuncParamOut <* skip) <*>
    (parseString symbolFuncReturn *> skip *> (parseType <|> fatal "Lambda" "Invalid Return Type") <* skip) <*>
    ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Lambda" "invalid body")

parseValue :: Parser AExpression
parseValue =
  skip
    *> ( AValue
           <$> ( parseStringValue
                   <|> parseNumberValue
                   <|> parseTupleValue
                   <|> parseArrayValue
                   <|> parseVectorValue
                   <|> parseStructValue
                   <|> parseVarCall
                   <|> parseLambda
               )
       )
    <* skip

parseInfix :: Parser AExpression
parseInfix =
  skip
    *> ( (\e1 n e2 -> ACall n (catMaybes [e1, Just e2]))
           <$> optional parseBasicExpression
           <* skip
           <*> (AValue <$> AVarCall <$> infixSymbol)
           <* skip
           <*> parseExpression
       )
  where
    infixSymbol = parseBetween symbolInfix <|> ((: []) <$> parseAnyChar allowedInfix)

parseMethodCall :: Parser AExpression
parseMethodCall =
    skip
    *> ( AMethodCall
                <$> ((parseChar symbolExpressionIn *> parseBasicExpression <* parseChar symbolExpressionOut) <|>
                    parseValue
                ) <* skip
                <*>  (parseChar symbolCallMethod *> skip *> parseName <* skip)
                <*> (parseChar symbolCallIn *> parseMultiple parseExpression <* parseChar symbolCallOut)
        )
    <* skip

parseCall :: Parser AExpression
parseCall =
  skip
    *> ( ACall
             <$> ((parseChar symbolExpressionIn *> parseBasicExpression <* parseChar symbolExpressionOut) <|>
                 parseValue
             )
             <* skip
             <*> (parseChar symbolCallIn *> parseMultiple parseExpression <* parseChar symbolCallOut)
       )
    <* skip

parseWrapperAccess :: (String -> AExpression -> AstAccess) -> Char -> Char -> Parser AstAccess
parseWrapperAccess f i o = f <$> (parseName <* skip) <*> (parseChar i *> parseExpression <* parseChar o)

parseAccess :: Parser AExpression
parseAccess =
  AAccess
    <$> ( skip
            *> ( parseWrapperAccess AArrayAccess symbolArrayIn symbolArrayOut
                   <|> parseWrapperAccess AVectorAccess symbolVectorIn symbolVectorOut
                   <|> parseWrapperAccess ATupleAccess symbolTuple symbolTuple
                   <|> AStructAccess
                           <$> (parseName <* skip)
                           <*> ( parseChar symbolStructIn
                                   *> parseMultiple (skip *> parseName <* skip)
                                   <* skip
                                   <* parseChar symbolStructOut
                               )
               )
        )

parseBasicExpression :: Parser AExpression
parseBasicExpression =
  skip
    *> (    parseCall
        <|> parseMethodCall
        <|> (parseChar symbolExpressionIn *> parseExpression <* parseChar symbolExpressionOut)
        <|> parseAccess
        <|> parseValue
       )
    <* skip

parseExpression :: Parser AExpression
parseExpression = parseInfix <|> parseBasicExpression

parseLineExpression :: Parser AExpression
parseLineExpression = skip *> parseExpression <* parseChar symbolEndOfExpression




------------------------------------------------
-- Block Parsing
------------------------------------------------
parseWhile :: Parser Ast
parseWhile =
  parseString symbolWhile
    *> ( ALoop Nothing
           <$> (parseChar symbolForIn *> (AExpress <$> parseExpression) <* parseChar symbolForOut)
           <*> pure Nothing
           <*> parseBody
       )

parseFor :: Parser Ast
parseFor =
  parseString symbolFor
    *> skip
    *> parseChar symbolForIn
    *> ( ALoop
           <$> optional parseDeclaration
           <*> ((parseChar symbolForSep *> (AExpress <$> parseExpression)) <|> fatal "For" "invalid condition")
           <*> (parseChar symbolForSep *> optional (AExpress <$> parseExpression))
           <*> ((parseChar symbolForOut <|> fatal "For" ("missing char \"" <> [symbolForOut] <> "\"")) *> skip *> (parseBody <|> fatal "For" "invalid body"))
       )

parseForIn :: Parser Ast
parseForIn =
  parseString symbolFor
    *> skip
    *> ( AForIn
           <$> (parseName <|> fatal "For" "missing variable or condition")
           <* skip
           <* parseString symbolIn
           <*> ((AExpress <$> parseExpression) <|> fatal "For In" "invalid expression")
           <*> (parseBody <|> fatal "For In" "invalid body")
       )

parseCond :: Parser Ast
parseCond =
  skip *> parseChar symbolCondIn *> skip *> (AExpress <$> parseExpression) <* skip <* parseChar symbolCondOut <* skip

parseIf :: Parser Ast
parseIf =
  AIf
    <$> (skip *> parseString symbolIf *> (parseCond <|> fatal "If" "invalid condition"))
    <*> ((skip *> parseBody) <|> fatal "If" "invalid code block")
    <*> optional (parseString symbolElse *> skip *> (parseBody <|> parseIf <|> fatal "Else" "invalid else body"))

parseFunction :: Parser Ast
parseFunction =
    f <$> ((,,,)
    <$> (skip *> parseString symbolFunc *> skip *> parseName)
    <*> ( skip
            *> (parseChar symbolFuncParamIn <|> fatal "Function" ("missing char \"" <> [symbolFuncParamIn] <> "\""))
            *> parseMultiple parseDeclaration
            <* (parseChar symbolFuncParamOut <|> fatal "Function" ("missing char \"" <> [symbolFuncParamOut] <> "\""))
        )
    <*> ((skip *> parseString symbolFuncReturn *> parseType <* skip) <|> fatal "Function" ("missing return value type after \"" <> symbolFuncReturn <> " symbol\""))
    <*> ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Function" "invalid body"))
    where
        f (name, args, ret, body) = AVarDecl
            (TKonst (TFunc (map getVarType args) ret))
            name
            (Just (AValue (ALambda args ret body)))




------------------------------------------------
-- BaseParsing
------------------------------------------------
parseAstBlockContent :: Parser Ast
parseAstBlockContent =
  parseIf
    <|> parseWhile
    <|> parseFor
    <|> parseForIn
    <|> parseReturn
    <|> parseAstFile
    <|> AExpress <$> parseLineExpression

parseReturn :: Parser Ast
parseReturn = parseString symbolReturn *>
    ((AReturn . AExpress <$> parseLineExpression) <|> fatal "Return" "invalid expression")

parseAstBlock :: Parser [Ast]
parseAstBlock =
  many $
    skip
      *> parseAstBlockContent
      <* skip

parseAstFile :: Parser Ast
parseAstFile =
  skip
    *> parseFunction
    <|> parseInclude
    <|> parseLineDeclaration
    <|> parseStruct
    <|> parseTrait
    <|> parseTraitImpl
      <* skip

parseBody :: Parser Ast
parseBody =
  ABlock
    <$> (skip *> parseChar symbolBlockIn *> parseAstBlock <* skip <* parseChar symbolBlockOut <* skip)

parseAst :: Parser [Ast]
parseAst = many parseAstFile
