module AstParsing.Keywords.Keywords where

symbolFunc :: String
symbolFunc = "Funk"

symbolStruct :: String
symbolStruct = "Strukt"

symbolStructIn :: String
symbolStructIn = "{"

symbolStructOut :: String
symbolStructOut = "}"

symbolStructSep :: Char
symbolStructSep = ';'

symbolInclude :: String
symbolInclude = "Inklude"

symbolIncludeIn :: String
symbolIncludeIn = "("

symbolIncludeOut :: String
symbolIncludeOut = ")"

symbolIncludeSep :: Char
symbolIncludeSep = ','

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

symbolVecIn :: Char
symbolVecIn = '<'

symbolVecOut :: Char
symbolVecOut = '>'

symbolArrayIn :: Char
symbolArrayIn = '['

symbolArrayOut :: Char
symbolArrayOut = ']'

symbolTuple :: Char
symbolTuple = '|'

wrapperList :: String
wrapperList = "<|["

symbolTrue :: String
symbolTrue = "true"

symbolFalse :: String
symbolFalse = "false"

symbolLineComment :: String
symbolLineComment = "//"

symbolCommentIn :: String
symbolCommentIn = "/*"

symbolCommentOut :: String
symbolCommentOut = "*/"
