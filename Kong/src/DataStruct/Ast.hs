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
    AstAccess (..),
  )
where

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
  | TArray Type AExpression -- Type[size]
  | TVector Type AExpression-- Type<size>
  | TTuple [Type]           -- |Type1, Type2, ...|
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
  | AArray [AExpression]
  | AVector [AExpression]
  | AStruct [(String, AExpression)]
  | AVarCall String
  deriving Show

data AstAccess
    = AArrayAccess {
        aVarName :: String,
        aIndex :: AExpression
    }
    | AVectorAccess {
        vVarName :: String,
        vIndex :: AExpression
    }
    | ATupleAccess {
        tVarName :: String,
        tIndex :: AExpression
    }
    | AStructAccess {
        sVarName :: String,
        fields :: [String]
    }
    deriving Show

data AExpression
    = AValue AstValue
    | AAccess AstAccess
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
      { includeFrom :: String,
        includeItems :: [String]
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
        ifElse :: Maybe Ast
      }
  | AReturn {returnValue :: Ast}
  | ABlock [Ast]
  | ALoop
      { loopInit :: Maybe Ast,
        loopCond :: Ast,
        loopIncr :: Maybe Ast,
        loopBody :: Ast
      }
  | AForIn
      { forInVar :: String,
        forInIter :: Ast,
        forInBody :: Ast
      }
  deriving Show
