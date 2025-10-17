{-# OPTIONS_GHC -Wno-partial-fields #-}

module DataStruct.Ast
  ( AstInt,
    AstSymbol,
    AstNumber (..),
    AstBool,
    AstChar,
    AstFloat,
    Type (..),
    AstValue (..),
    Ast (..),
    AExpression (..),
  )
where

import Parser (LineCount)

-- Basic types
type AstInt = Int
type AstSymbol = String
type AstBool = Bool
type AstChar = Char
type AstFloat = Float

-- Type system
data Type
  = TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TKonst Type
  | TStrong Type            -- long modifier
  | TKong Type              -- unsigned modifier
  | TStruct String          -- struct type by name
  | TTrait String           -- trait type by name
  | TArray Type Int         -- Type[size]
  | TVector Type AExpression-- Type<size>
  | TList Type Int          -- Type[size]
  | TTuple [Type]           -- (Type1, Type2, ...)
  | TCustom String          -- custom type alias
  deriving Show

data AstNumber
    = AInteger AstInt
    | ABool AstBool
    | AChar AstChar
    | AFloat AstFloat
    deriving Show

data AstValue
  = ANumber AstNumber
  | AString String
  | ATuple [AExpression]
  | AVarCall String
  deriving Show

data AExpression
    = AValue AstValue
    | AAttribution
        { variable :: String,
          value :: AExpression
        }
    | ACall
        { callFunction :: String,
          callArgs :: [AExpression]
        }
    deriving Show


-- Main AST
data Ast
  -- Declarations & Definitions
  = AFunkDef
      { funkName :: String,
        funkParams :: [Ast],
        funkReturnType :: Type,
        funkBody :: [Ast]
      }
  | AExpress AExpression
  | AStruktDef
      { struktName :: String,
        struktFields :: [(Type, String)]
      }
  | ATypeAlias
      { aliasName :: String,
        aliasType :: Type
      }
  | AVarDecl
      {
        varType :: Type,
        varName :: String,
        varValue :: Maybe AExpression
      }
  | AInclude
      { includeItems :: [String],
        includeFrom :: String
      }
  -- Expressions
  | ASymbol AstSymbol
  | ALambda
      { lambdaParams :: [(Type, String)],
        lambdaReturnType :: Type,
        lambdaBody :: Ast
      }
  -- Control Flow
  | AIf
      { ifCond :: Ast,
        ifThen :: Ast,
        ifElifs :: [(Ast, Ast)],  -- [(condition, body)]
        ifElse :: Maybe Ast
      }
  | AReturn {returnValue :: Ast}
  | ABlock [Ast]
  | AWhile
      { whileCond :: Ast,
        whileBody :: Ast
      }
  | AFor
      { forInit :: Ast,
        forCond :: Ast,
        forIncr :: Ast,
        forBody :: Ast
      }
  | AForIn
      { forInVar :: String,
        forInIter :: Ast,
        forInBody :: Ast
      }
  -- Access Operations
  | AArrayAccess
      { arrayExpr :: Ast,
        arrayIndex :: Ast
      }
  | AVectorAccess
      { vectorExpr :: Ast,
        vectorIndex :: Ast
      }
  | ATupleAccess
      { tupleExpr :: Ast,
        tupleIndex :: Int
      }
  | AStructAccess
      { structExpr :: Ast,
        structFields :: [String]
      }
  deriving Show
