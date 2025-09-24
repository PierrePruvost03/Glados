{-# LANGUAGE LambdaCase #-}

module Interpreter.EvalAst
  ( astValueToValue
  , evalAst
  ) where

import DataStruct.Ast (AstValue(..), Ast(..))
import Interpreter.BaseEnv (Value(..), Env, lookupEnv, err)
import Parser (LineCount)

astValueToValue :: (LineCount, AstValue) -> Value
astValueToValue (_, AstInteger i) = VInt i
astValueToValue (_, AstBool b) = VBool b
astValueToValue (_, AstString s) = VString s

evalAst :: Env -> Ast -> Either String Value
evalAst env = \case
  AValue val -> Right $ astValueToValue val
  ASymbol (lc, s) -> maybe (err lc $ "Unbound variable: " ++ s) Right (lookupEnv s env)
  AList (_, xs) -> VList <$> traverse (evalAst env) xs
  _ -> Left "Not implemented yet"
