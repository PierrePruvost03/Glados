module Interpreter.EvalAst
  ( astValueToValue
  ) where

import DataStruct.Ast (AstValue(..))
import Interpreter.BaseEnv (Value(..))
import Parser (LineCount)

astValueToValue :: (LineCount, AstValue) -> Value
astValueToValue (_, AstInteger i) = VInt i
astValueToValue (_, AstBool b) = VBool b
astValueToValue (_, AstString s) = VString s
