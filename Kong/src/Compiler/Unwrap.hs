{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Unwrap
  ( Unwrappable(..)
  , HasLineCount(..)
  ) where

import DataStruct.Ast
import Parser (LineCount)

-- Type class for unwrapping AST nodes
class Unwrappable a b | a -> b where
  unwrap :: a -> b

-- Type class for extracting line count from AST nodes
class HasLineCount a where
  lc :: a -> LineCount

instance Unwrappable Ast AstRaw where
  unwrap (_, raw) = raw

instance HasLineCount Ast where
  lc (lineCount, _) = lineCount

instance Unwrappable AExpression AExpressionRaw where
  unwrap (_, raw) = raw

instance HasLineCount AExpression where
  lc (lineCount, _) = lineCount

instance Unwrappable Type TypeRaw where
  unwrap (_, raw) = raw

instance HasLineCount Type where
  lc (lineCount, _) = lineCount

instance Unwrappable AstAccess AstAccessRaw where
  unwrap (_, raw) = raw

instance HasLineCount AstAccess where
  lc (lineCount, _) = lineCount

instance Unwrappable AstValue AstValueRaw where
  unwrap (_, raw) = raw

instance HasLineCount AstValue where
  lc (lineCount, _) = lineCount
