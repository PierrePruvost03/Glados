{-# LANGUAGE NamedFieldPuns #-}

module AstParsing.BaseParsing (parseAst) where

import AstParsing.Include
import AstParsing.Keywords.Keywords
import AstParsing.Skip
import AstParsing.Utils
import Control.Applicative
import Data.Functor
import Data.Maybe
import DataStruct.Ast
import Parser

------------------------------------------------
-- Type Declaration
------------------------------------------------
parseField :: Parser (Type, String)
parseField = (,) <$> parseType <*> (skip *> (parseName <|> fatal "Struct" "missing field's name"))

parseStruct :: Parser Ast
parseStruct =
  wrap $
    AStruktDef
      <$> (parseString symbolStruct *> (parseName <|> fatal "Struct" "missing struct name"))
      <*> ( skip
              *> (parseChar symbolStructIn <|> fatal "Struct" ("missing char \"" <> [symbolStructIn] <> "\""))
              *> many (skip *> parseField <* skip <* (parseChar symbolStructSep <|> fatal "Struct" ("missing char \"" <> [symbolStructSep] <> "\"")) <* skip)
              <* parseChar symbolStructOut
          )

parseTraitBody :: Parser [(String, [Type], Type)]
parseTraitBody =
  ( (parseChar symbolBlockIn <|> fatal "Trait" ("missing char \"" <> [symbolBlockIn] <> "\""))
      *> skip
      *> many
        ( (,,)
            <$> (skip *> (parseName) <* skip)
            <*> (parseChar symbolFuncParamIn *> parseMultiple parseType <* skip <* parseChar symbolFuncParamOut)
            <*> (skip *> parseString symbolFuncReturn *> skip *> parseType <* skip <* parseChar symbolStructSep)
            <* skip
        )
      <* (parseChar symbolBlockOut <|> fatal "Trait" ("missing char \"" <> [symbolBlockOut] <> "\""))
  )

parseTrait :: Parser Ast
parseTrait =
  wrap $
    skip
      *> ( ATraitDef
             <$> (parseString symbolTrait *> skip *> (parseName <|> fatal "Trait" "invalid trait name") <* skip)
             <*> (parseTraitBody)
         )

parseTraitFuncDef :: Type -> Parser Ast
parseTraitFuncDef t =
  wrap $
    f
      <$> wrap
        ( (,,,,)
            <$> (skip *> parseName)
            <*> ( skip
                    *> (parseChar symbolFuncParamIn <|> fatal "Method" ("missing char \"" <> [symbolFuncParamIn] <> "\""))
                    *> skip
                    *> ((parseChar symbolRef *> pure True) <|> pure False)
                    <* skip
                    <* (parseString symbolSelf <|> fatal "Method" "missing self declaration")
                    <* skip
                    <* parseChar ','
                )
            <*> ( skip
                    *> parseMultiple (parseTraitDeclaration t)
                    <* (parseChar symbolFuncParamOut <|> fatal "Method" ("missing char \"" <> [symbolFuncParamOut] <> "\""))
                )
            <*> ((skip *> parseString symbolFuncReturn *> parseTraitType t <* skip) <|> fatal "Method" ("missing return value type after \"" <> symbolFuncReturn <> " symbol\""))
            <*> ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Method" "invalid body")
        )
  where
    f (lc, (name, isRef, args, ret, body)) =
      AVarDecl
        (lc, TKonst (lc, TFunc (selfType isRef : (map (\(lcArg, v) -> (lcArg, getVarType v))) args) ret))
        (typeRawToSimpleString (snd t) <> ('$' : name))
        (Just (lc, (AValue (lc, ALambda ((lc, AVarDecl (selfType isRef) symbolSelf Nothing) : args) ret body))))
      where
        selfType True = (lc, TRef t)
        selfType False = t

parseTraitImpl :: Parser Ast
parseTraitImpl =
  skip
    *> ( (,) <$> (parseString symbolImpl *> skip *> parseName <* skip) <*> (parseType <* skip)
           >>= \(n, t) ->
             wrap $
               ATraitImpl
                 <$> pure n
                 <*> pure t
                 <*> ( (parseChar symbolBlockIn <|> fatal "Trait" ("missing char \"" <> [symbolBlockIn] <> "\""))
                         *> skip
                         *> many (skip *> parseTraitFuncDef t)
                         <* skip
                         <* (parseChar symbolBlockOut <|> fatal "Trait" ("missing char \"" <> [symbolBlockOut] <> "\""))
                     )
       )
    <* skip

------------------------------------------------
-- Declaration
------------------------------------------------
parseTraitDeclaration :: Type -> Parser Ast
parseTraitDeclaration t =
  wrap $
    skip
      *> ( AVarDecl
             <$> parseTraitType t
             <*> parseName
             <*> optional (parseChar symbolDeclaration *> parseExpression)
         )
      <* skip

parseDeclaration :: Parser Ast
parseDeclaration =
  wrap $
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
  where
    f =
      wrap $
        skip
          *> ( TKong <$> (parseString symbolUnsigned *> f)
                 <|> TStrong <$> (parseString symbolLong *> f)
                 <|> TInt <$ parseString symbolInt
                 <|> TBool <$ parseString symbolBool
                 <|> TChar <$ parseString symbolChar
                 <|> TFloat <$ parseString symbolFloat
                 <|> (getLineCount >>=
                     \lc -> TVector (lc, TChar) (lc, AValue (lc, ANumber (AInteger 0))) <$ parseString symbolString)
                 <|> TTuple
                   <$> ( parseChar symbolTuple
                           *> parseMultiple parseType
                           <* parseChar symbolTuple
                       )
                 <|> TCustom <$> parseName
                 <|> TFunc
                   <$> ( parseChar symbolFuncParamIn
                           *> parseMultiple parseType
                           <* ( parseChar symbolFuncParamOut
                                  <|> fatal "Function" ("missing char \"" <> [symbolFuncParamOut] <> "\"")
                              )
                           <* skip
                       )
                   <*> (parseString symbolFuncReturn *> skip *> parseType)
             )
          <* skip

parseConstType :: Parser Type -> Parser Type
parseConstType base =
  skip
    *> ( wrap (TKonst <$> (base <* skip <* parseString symbolConst))
           <|> base
       )
    <* skip

isNotWrapper :: Parser ()
isNotWrapper = isAnyNotChar wrapperList

parseParentType :: Parser Type -> Parser Type
parseParentType base = parseConstType (skip *> f <* skip)
  where
    f =
      wrap $
        TRef <$> (base <* skip <* parseChar symbolRef)
          <|> TVector
            <$> (base <* skip)
            <*> ( parseChar symbolVectorIn
                    *> ( parseExpression
                           <|> wrap (AValue <$> (wrap (pure (ANumber (AInteger 0)))))
                       )
                    <* (parseChar symbolVectorOut <|> fatal "Vector" ("missing char \"" <> [symbolVectorOut] <> "\""))
                )
          <|> TArray
            <$> (base <* skip)
            <*> ( parseChar symbolArrayIn
                    *> skip
                    *> parseExpression
                    <* skip
                    <* (parseChar symbolArrayOut <|> fatal "Array" ("missing char \"" <> [symbolArrayOut] <> "\""))
                )

parseRecParentType :: Parser Type -> Parser Type
parseRecParentType base =
  parseTry base
    *> skip
    *> ( (base <* skip <* isNotWrapper)
           <|> parseRecParentType (parseParentType base)
       )
    <* skip

parseTraitType :: Type -> Parser Type
parseTraitType t = parseRecParentType $ (skip *> parseString symbolSelfType *> pure t) <|> parseBaseType

parseType :: Parser Type
parseType = parseRecParentType parseBaseType

------------------------------------------------
-- Expressions
------------------------------------------------
parseStringValue :: Parser AstValue
parseStringValue = wrap $ AVector <$> (f <$> getLineCount <*> parseBetween symbolStringDelimiter)
    where
        f lc l = map (\c -> (lc, AValue (lc, ANumber (AChar c)))) l

parseNumberValue :: Parser AstValue
parseNumberValue =
  wrap $
    ANumber
      <$> (parseAstFloat <|> parseAstInt <|> parseAstBool <|> parseAstChar)
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
    <* (parseChar o <|> fatal "Parsing" ("missing char \"" <> [o] <> "\""))

parseTupleValue :: Parser AstValue
parseTupleValue = wrap $ ATuple <$> parseWrapper symbolTuple symbolTuple

parseArrayValue :: Parser AstValue
parseArrayValue = wrap $ AArray <$> parseWrapper symbolArrayIn symbolArrayOut

parseVectorValue :: Parser AstValue
parseVectorValue = wrap $ AVector <$> parseWrapper symbolVectorIn symbolVectorOut

parseStructValue :: Parser AstValue
parseStructValue =
  wrap $
    AStruct
      <$> ( parseChar symbolStructIn
              *> parseMultiple
                ( (,)
                    <$> parseName
                    <* skip
                    <* (parseChar symbolDeclaration <|> fatal "Struct Value" ("missing char \"" <> [symbolDeclaration] <> "\""))
                    <* skip
                    <*> (parseExpression <|> fatal "Struct Value" "invalid expression")
                )
              <* (parseChar symbolStructOut <|> fatal "Struct Value" ("missing char \"" <> [symbolStructOut] <> "\""))
          )

parseVarCall :: Parser AstValue
parseVarCall = wrap $ AVarCall <$> parseName

parseLambda :: Parser AstValue
parseLambda =
  wrap $
    ALambda
      <$> (parseChar symbolFuncParamIn *> parseMultiple parseDeclaration <* parseChar symbolFuncParamOut <* skip)
      <*> (parseString symbolFuncReturn *> skip *> (parseType <|> fatal "Lambda" "Invalid Return Type") <* skip)
      <*> ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Lambda" "invalid body")

parseValue :: Parser AExpression
parseValue =
  wrap $
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
  wrap $
    skip
      *> ( (\e1 n e2 -> ACall n (catMaybes [e1, Just e2]))
             <$> optional parseBasicExpression
             <* skip
             <*> wrap (AValue <$> wrap (AVarCall <$> infixSymbol))
             <* skip
             <*> parseExpression
         )
  where
    infixSymbol = parseBetween symbolInfix <|> ((: []) <$> parseAnyChar allowedInfix)

parseWrappedExpression :: Parser AExpression
parseWrappedExpression = (parseChar symbolExpressionIn *> parseBasicExpression <* parseChar symbolExpressionOut)

parseMethodCall :: Parser AExpression
parseMethodCall =
  wrap $
    skip
      *> ( AMethodCall
             <$> ( parseWrappedExpression
                     <|> parseValue
                 )
             <* skip
             <*> (parseChar symbolCallMethod *> skip *> parseName <* skip)
             <*> (parseChar symbolCallIn *> parseMultiple parseExpression <* parseChar symbolCallOut)
         )
      <* skip

parseCall :: Parser AExpression
parseCall =
  wrap $
    skip
      *> ( ACall
             <$> ( parseWrappedExpression
                     <|> parseValue
                 )
             <* skip
             <*> (parseChar symbolCallIn *> parseMultiple parseExpression <* parseChar symbolCallOut)
         )
      <* skip

parseWrapperAccess :: (AExpression -> AExpression -> AstAccessRaw) -> Char -> Char -> Parser AstAccessRaw
parseWrapperAccess f i o = f <$> (parseWrappedExpression <|> parseValue) <*> (parseChar i *> parseExpression <* parseChar o)

parseAccess :: Parser AExpression
parseAccess =
  wrap $
    AAccess
      <$> ( skip
              *> ( wrap (parseWrapperAccess AArrayAccess symbolArrayIn symbolArrayOut)
                     <|> wrap (parseWrapperAccess AVectorAccess symbolVectorIn symbolVectorOut)
                     <|> wrap (parseWrapperAccess ATupleAccess symbolTuple symbolTuple)
                     <|> wrap
                       ( AStructAccess
                           <$> ((parseWrappedExpression <|> parseValue) <* skip)
                           <*> ( parseChar symbolStructIn
                                   *> parseMultiple (skip *> parseName <* skip)
                                   <* skip
                                   <* parseChar symbolStructOut
                               )
                       )
                 )
          )

parseCast :: Parser AExpression
parseCast =
  wrap $
    ACast
      <$> ( skip
              *> parseString symbolCast
              *> skip
              *> skip
              *> parseChar symbolFuncParamIn
              *> parseType
          )
      <*> (skip *> parseChar ',' *> parseExpression <* parseChar symbolFuncParamOut)

parseBasicExpression :: Parser AExpression
parseBasicExpression =
  skip
    *> ( parseCast
           <|> parseCall
           <|> parseMethodCall
           <|> parseAccess
           <|> (parseChar symbolExpressionIn *> parseExpression <* parseChar symbolExpressionOut)
           <|> parseValue
       )
    <* skip

parseExpression :: Parser AExpression
parseExpression = parseInfix <|> parseBasicExpression

parseLineExpression :: Parser AExpression
parseLineExpression = skip *> parseExpression <* parseChar symbolEndOfExpression

------------------------a------------------------
-- Block Parsing
------------------------------------------------
parseWhile :: Parser Ast
parseWhile =
  wrap $
    skip
      *> parseString symbolWhile
      *> skip
      *> ( ALoop Nothing
             <$> (parseChar symbolForIn *> wrap (AExpress <$> parseExpression) <* skip <* parseChar symbolForOut)
             <*> pure Nothing
             <*> (skip *> parseBody)
         )

parseFor :: Parser Ast
parseFor =
  wrap $
    parseString symbolFor
      *> skip
      *> parseChar symbolForIn
      *> ( ALoop
             <$> optional parseDeclaration
             <*> ((parseChar symbolForSep *> wrap (AExpress <$> parseExpression)) <|> fatal "For" "invalid condition")
             <*> (parseChar symbolForSep *> optional (wrap (AExpress <$> parseExpression)))
             <*> ((parseChar symbolForOut <|> fatal "For" ("missing char \"" <> [symbolForOut] <> "\"")) *> skip *> (parseBody <|> fatal "For" "invalid body"))
         )

parseForIn :: Parser Ast
parseForIn =
  wrap $
    parseString symbolFor
      *> skip
      *> ( AForIn
             <$> (parseName <|> fatal "For" "missing variable or condition")
             <* skip
             <* parseString symbolIn
             <*> (wrap (AExpress <$> parseExpression) <|> fatal "For In" "invalid expression")
             <*> (parseBody <|> fatal "For In" "invalid body")
         )

parseCond :: Parser Ast
parseCond =
  wrap $
    skip *> parseChar symbolCondIn *> skip *> (AExpress <$> parseExpression) <* skip <* parseChar symbolCondOut <* skip

parseIf :: Parser Ast
parseIf =
  wrap $
    AIf
      <$> (skip *> parseString symbolIf *> (parseCond <|> fatal "If" "invalid condition"))
      <*> ((skip *> parseBody) <|> fatal "If" "invalid code block")
      <*> optional (parseString symbolElse *> skip *> (parseBody <|> parseIf <|> fatal "Else" "invalid else body"))

parseFunction :: Parser Ast
parseFunction =
  wrap $
    f
      <$> wrap
        ( ( (,,,)
              <$> (skip *> parseString symbolFunc *> skip *> parseName)
              <*> ( skip
                      *> (parseChar symbolFuncParamIn <|> fatal "Function" ("missing char \"" <> [symbolFuncParamIn] <> "\""))
                      *> parseMultiple parseDeclaration
                      <* (parseChar symbolFuncParamOut <|> fatal "Function" ("missing char \"" <> [symbolFuncParamOut] <> "\""))
                  )
              <*> ((skip *> parseString symbolFuncReturn *> parseType <* skip) <|> fatal "Function" ("missing return value type after \"" <> symbolFuncReturn <> " symbol\""))
              <*> ((parseChar symbolBlockIn *> parseAstBlock <* parseChar symbolBlockOut) <|> fatal "Function" "invalid body")
          )
        )
  where
    f (lc, (name, args, ret, body)) =
      AVarDecl
        (lc, TKonst (lc, TFunc (map (\(lcArg, v) -> (lcArg, getVarType v)) args) ret))
        name
        (Just (lc, (AValue (lc, ALambda args ret body))))

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
    <|> wrap (AExpress <$> parseLineExpression)

parseReturn :: Parser Ast
parseReturn =
  wrap $
    parseString symbolReturn
      *> ((AReturn <$> wrap (AExpress <$> parseLineExpression)) <|> fatal "Return" "invalid expression")

parseAstBlock :: Parser [Ast]
parseAstBlock =
  many $
    skip
      *> parseAstBlockContent
      <* skip

parseAstFile :: Parser Ast
parseAstFile =
  skip
    *> ( parseFunction
           <|> parseInclude
           <|> parseLineDeclaration
           <|> parseStruct
           <|> parseTrait
           <|> parseTraitImpl
       )
    <* skip

parseBody :: Parser Ast
parseBody =
  wrap $
    ABlock
      <$> (skip *> parseChar symbolBlockIn *> parseAstBlock <* skip <* parseChar symbolBlockOut <* skip)

parseAst :: Parser [Ast]
parseAst = many parseAstFile

-- Helper function to convert TypeRaw to simple string (without T prefix)
typeRawToSimpleString :: TypeRaw -> String
typeRawToSimpleString TInt = "Int"
typeRawToSimpleString TBool = "Bool"
typeRawToSimpleString TChar = "Char"
typeRawToSimpleString TString = "String"
typeRawToSimpleString TFloat = "Float"
typeRawToSimpleString (TStruct name) = name
typeRawToSimpleString (TTrait name) = name
typeRawToSimpleString (TKonst t) = typeRawToSimpleString (snd t)
typeRawToSimpleString (TStrong t) = typeRawToSimpleString (snd t)
typeRawToSimpleString (TKong t) = typeRawToSimpleString (snd t)
typeRawToSimpleString (TRef t) = "&" ++ typeRawToSimpleString (snd t)
typeRawToSimpleString (TArray t _) = typeRawToSimpleString (snd t) ++ "[]"
typeRawToSimpleString (TVector t _) = typeRawToSimpleString (snd t) ++ "<>"
typeRawToSimpleString (TTuple ts) = "|" ++ unwords (map (typeRawToSimpleString . snd) ts) ++ "|"
typeRawToSimpleString (TCustom name) = name
typeRawToSimpleString (TFunc args ret) = "(" ++ unwords (map (typeRawToSimpleString . snd) args) ++ "->" ++ typeRawToSimpleString (snd ret) ++ ")"