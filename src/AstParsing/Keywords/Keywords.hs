module AstParsing.Keywords.Keywords where

symbolFunc :: String
symbolFunc = "Funk"

symbolConst :: String
symbolConst = "Konst"

symbolUnsigned :: String
symbolUnsigned = "Kong"

symbolLong :: String
symbolLong = "Strong"

symbolInt :: String
symbolInt = "Int"

symbolFloat :: String
symbolFloat = "Float"

symbolBool :: String
symbolBool = "Bool"

symbolChar :: String
symbolChar = "Char"

symbolString :: String
symbolString = "String"

symbolVectorIn :: Char
symbolVectorIn = '<'

symbolVectorOut :: Char
symbolVectorOut = '>'

symbolArrayIn :: Char
symbolArrayIn = '['

symbolArrayOut :: Char
symbolArrayOut = ']'

symbolStructIn :: Char
symbolStructIn = '{'

symbolStructOut :: Char
symbolStructOut = '}'

symbolTuple :: Char
symbolTuple = '|'

wrapperList :: String
wrapperList = "<|["

symbolTrue :: String
symbolTrue = "true"

symbolFalse :: String
symbolFalse = "false"

allowedInfix :: String
allowedInfix = "+-/:*^!%?><&|"
