{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Interpreter.EvalAst
  ( astValueToValue
  , evalAst
  ) where

import DataStruct.Ast (AstValue(..), Ast(..), AstLambda(..))
import Interpreter.BaseEnv (Value(..), Env, lookupEnv, extendEnv)
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
evalAst env (ALambdas (_, AstLambda params body)) =
  VLambda <$> traverse getParam params <*> pure body <*> pure env
  where
    getParam (ASymbol (_, s)) = Right s
    getParam x = Left $ "Invalid parameter: " ++ show x
evalAst env (AIf (_, cond) (_, t) (_, e)) =
  evalAst env cond >>= \v ->
    case v of
      VBool True -> evalAst env t
      VBool False -> evalAst env e
      _ -> Left "Condition is not a boolean"
evalAst _ _ = Left "Not implemented yet"
