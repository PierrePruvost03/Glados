{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Interpreter.EvalAst
  ( astValueToValue
  , evalAst
  ) where

import DataStruct.Ast (AstValue(..), Ast(..), AstLambda(..))
import Interpreter.BaseEnv (lookupEnv, extendEnv)
import DataStruct.Value (Value(..), Env)
import Parser (LineCount)
import Control.Monad (foldM)

astValueToValue :: (LineCount, AstValue) -> Value
astValueToValue (_, AstInteger i) = VInt i
astValueToValue (_, AstBool b) = VBool b
astValueToValue (_, AstString s) = VString s

evalAst :: Env -> Ast -> Either String (Value, Env)
evalAst env (AValue v) = Right (astValueToValue v, env)
evalAst env (ASymbol (lineCount, s)) =
    case lookupEnv s env of
      Just val -> Right (val, env)
      Nothing -> Left $ "Unbound variable: " ++ s ++ " at " ++ show lineCount
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
    getParam x = Left $ "Invalid parameter: " ++ show x ++ " at " ++ show lineCount
evalAst env (AIf (lineCount, cond) (_, t) (_, e)) =
  evalAst env cond >>= \(condVal, _) ->
    case condVal of
      VBool True -> evalAst env t
      VBool False -> evalAst env e
      _ -> Left $ "Condition is not a boolean at " ++ show lineCount
evalAst env (ADefine (_, varName) (_, v)) =
  evalAst env v >>= \(val, _) ->
    Right (val, extendEnv env [(varName, val)])
evalAst env (ACall (lineCount, funcName) (_, arg)) =
  case lookupEnv funcName env of
    Nothing -> Left $ "Unbound function: " ++ funcName ++ " at " ++ show lineCount
    Just func -> callFunction funcName lineCount env func arg

callFunction :: String -> LineCount -> Env -> Value -> Ast -> Either String (Value, Env)
callFunction funcName lineCount callEnv (VLambda params body lambdaEnv) argAst =
  evaluateArgs callEnv argAst >>= \(argValues, _) ->
    case length params == length argValues of
      False -> Left $ "Wrong number of arguments: expected " ++ show (length params) 
                      ++ ", got " ++ show (length argValues) ++ " at " ++ show lineCount
      True -> evalAst (extendEnv (extendEnv lambdaEnv [(funcName, VLambda params body lambdaEnv)]) (zip params argValues)) body >>= \(result, _) ->
        Right (result, callEnv)
callFunction _ _ callEnv (VPrim _ primFunc) argAst =
  evaluateArgs callEnv argAst >>= \(argValues, newEnv) ->
    primFunc argValues >>= \result -> Right (result, newEnv)
callFunction funcName lineCount _ _ _ = Left $ funcName ++ " is not a function at " ++ show lineCount

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
