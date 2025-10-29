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
  | TFunc [Type] Type     -- arg type -> return type
  | TRef Type
  deriving (Show, Eq)

data AstNumber
    = AInteger AstInt
    | ABool AstBool
    | AChar AstChar
    | AFloat AstFloat
  deriving (Show, Eq)

data AstValue
  = ANumber AstNumber
  | AString String
  | ATuple [AExpression]
  | AArray [AExpression]
  | AVector [AExpression]
  | AStruct [(String, AExpression)]
  | AVarCall String
  | ALambda [Ast] Type [Ast]
  deriving (Show, Eq)

data AstAccess
    = AArrayAccess {
        aVarName :: AExpression,
        aIndex :: AExpression
    }
    | AVectorAccess {
        vVarName :: AExpression,
        vIndex :: AExpression
    }
    | ATupleAccess {
        tVarName :: AExpression,
        tIndex :: AExpression
    }
    | AStructAccess {
        sVarName :: AExpression,
        fields :: [String]
    }
  deriving (Show, Eq)

data AExpression
    = AValue AstValue
    | AAccess AstAccess
    | AAttribution
        { variable :: String,
          value :: AExpression
        }
    | ACall
        { callFunction :: AExpression,
          callArgs :: [AExpression]
        }
    | AMethodCall
        { calledVar :: AExpression,
          calledMethod :: String,
          callArgs :: [AExpression]
        }
    deriving (Show, Eq)


-- Main AST
data Ast
  -- Declarations & Definitions
  = AExpress AExpression
  | AStruktDef
      { struktName :: String,
        struktFields :: [(Type, String)]
      }
  | ATraitDef
    { traitName :: String,
      traitMethods :: [(String, [Type], Type)]
    }
  | ATraitImpl
    { implTrait :: String,
      traitType :: Type,
      implMethods :: [Ast]
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
  deriving (Show, Eq)
