module Interpreter.EvalAst
  ( astValueToValue
  , evalAst
  ) where

import DataStruct.Ast (AstValue(..), Ast(..))
import Interpreter.BaseEnv (Value(..), Env, lookupEnv)
import Parser (LineCount)

astValueToValue :: (LineCount, AstValue) -> Value
astValueToValue (_, AstInteger i) = VInt i
astValueToValue (_, AstBool b) = VBool b
astValueToValue (_, AstString s) = VString s

evalAst :: Env -> Ast -> Either String Value
evalAst _ (AValue v) = Right $ astValueToValue v
evalAst env (ASymbol (_, s)) =
    case lookupEnv s env of
      Just val -> Right val
      Nothing -> Left $ "Unbound variable: " ++ s
evalAst env (AList (_, xs)) = VList <$> traverse (evalAst env) xs
evalAst _ _ = Left "Not implemented yet"
