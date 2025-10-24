{-# LANGUAGE LambdaCase #-}

module Compiler.Expr
  ( compileExpr
  , compileCall
  , compileValue
  , compileAccess
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..), Env)
import Compiler.Types (CompilerError(..))
 

compileExpr :: AExpression -> Env -> Either CompilerError [Instr]
compileExpr expr env = case expr of
  AAttribution var rhs ->
    fmap (++ [SetVar var]) (compileExpr rhs env)
  ACall funcName args ->
    fmap (\argInstrs -> concat argInstrs ++ compileCall funcName)
         (mapM (`compileExpr` env) (reverse args))
  AValue astValue -> compileValue astValue env
  AAccess access -> compileAccess access env

compileCall :: String -> [Instr]
compileCall funcName =
  case funcName `elem` builtinOps of
    True -> [DoOp (stringToOp funcName)]
    False -> [PushEnv funcName, Call]

compileValue :: AstValue -> Env -> Either CompilerError [Instr]
compileValue val _env = case val of
  ANumber (AInteger n) -> Right [Push (VNumber (VInt n))]
  ANumber (AFloat f) -> Right [Push (VNumber (VFloat (realToFrac f)))]
  ANumber (ABool b) -> Right [Push (VNumber (VBool b))]
  ANumber (AChar c) -> Right [Push (VNumber (VChar c))]
  AString s -> Right $ map (Push . VNumber . VChar) s ++ [CreateList (length s)]
  AVarCall vname -> Right [PushEnv vname]
  _ -> Left $ UnsupportedAst ("Unsupported value: " ++ show val)

compileAccess :: AstAccess -> Env -> Either CompilerError [Instr]
compileAccess access env = case access of
  AArrayAccess arrName idx ->
    fmap (([PushEnv arrName] ++) . (++ [GetList])) (compileExpr idx env)
  _ -> Left $ UnsupportedAst ("Unsupported access: " ++ show access)
