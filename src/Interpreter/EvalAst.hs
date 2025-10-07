{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Interpreter.EvalAst
  ( astValueToValue,
    evalAst,
    throwError,
  )
where

import Control.Monad (foldM)
import DataStruct.Ast (Ast (..), AstLambda (..), AstValue (..))
import DataStruct.Value (Env, Value (..))
import Interpreter.Env.BaseEnv (extendEnv, lookupEnv)
import Parser (LineCount)

throwError :: LineCount -> String -> Either String a
throwError lc msg = Left $ "Error at " ++ show lc ++ ": " ++ msg

astValueToValue :: (LineCount, AstValue) -> Value
astValueToValue (_, AstInteger i) = VInt i
astValueToValue (_, AstBool b) = VBool b
astValueToValue (_, AstString s) = VString s

evalAst :: Env -> Ast -> Either String (Value, Env)
evalAst env (AValue v) = Right (astValueToValue v, env)
evalAst env (ASymbol (lineCount, s)) =
  case lookupEnv s env of
    Just val -> Right (val, env)
    Nothing -> throwError lineCount $ "Unbound variable: " ++ s
evalAst env (AList (_, xs)) =
  foldM evalListItem ([], env) xs >>= \(vals, newEnv) ->
    Right (VList (reverse vals), newEnv)
  where
    evalListItem (acc, currentEnv) ast =
      evalAst currentEnv ast >>= \(val, updatedEnv) ->
        Right (val : acc, updatedEnv)
evalAst env (ALambdas (lineCount, AstLambda params body)) =
  case traverse getParam params of
    Right paramNames -> Right (VLambda paramNames body env, env)
    Left err -> Left err
  where
    getParam (ASymbol (_, s)) = Right s
    getParam x = throwError lineCount $ "Invalid parameter: " ++ show x
evalAst env (AIf (lineCount, cond) (_, t) (_, e)) =
  evalAst env cond >>= \(condVal, _) ->
    case condVal of
      VBool True -> evalAst env t
      VBool False -> evalAst env e
      _ -> throwError lineCount "Condition is not a boolean"
evalAst env (ADefine (_, varName) (_, v)) =
  evalAst env v >>= \(val, _) ->
    Right (val, extendEnv env [(varName, val)])
evalAst env (ACall (lineCount, Left funcName) (_, arg)) =
  case lookupEnv funcName env of
    Nothing -> throwError lineCount $ "Unbound function: " ++ funcName
    Just func -> callFunction funcName lineCount env func arg
evalAst env (ACall (lineCount, Right func) (_, arg)) = case evalAst env (ALambdas (lineCount, func)) of
  Right (v, newEnv) -> callFunction "lambda" lineCount newEnv v arg
  Left err -> throwError lineCount err

callFunction :: String -> LineCount -> Env -> Value -> Ast -> Either String (Value, Env)
callFunction funcName lineCount callEnv (VLambda params body lambdaEnv) argAst =
  evaluateArgs callEnv argAst >>= \(argValues, _) -> case () of
    _
      | length params /= length argValues ->
          throwError lineCount $
            "Wrong number of arguments: expected "
              ++ show (length params)
              ++ ", got "
              ++ show (length argValues)
      | otherwise ->
          evalAst (extendEnv (extendEnv lambdaEnv [(funcName, VLambda params body lambdaEnv)]) (zip params argValues)) body
            >>= \(result, _) -> Right (result, callEnv)
callFunction _ _ callEnv (VPrim _ primFunc) argAst =
  evaluateArgs callEnv argAst >>= \(argValues, newEnv) ->
    primFunc argValues >>= \result -> Right (result, newEnv)
callFunction funcName lineCount _ _ _ = throwError lineCount $ funcName ++ " is not a function"

evaluateArgs :: Env -> Ast -> Either String ([Value], Env)
evaluateArgs argEnv (AList (_, argList)) =
  foldM evalArgItem ([], argEnv) argList >>= \(vals, finalEnv) ->
    Right (reverse vals, finalEnv)
  where
    evalArgItem (acc, currentEnv) ast =
      evalAst currentEnv ast >>= \(val, updatedEnv) ->
        Right (val : acc, updatedEnv)
evaluateArgs argEnv singleArg =
  evalAst argEnv singleArg >>= \(val, newEnv) ->
    Right ([val], newEnv)
