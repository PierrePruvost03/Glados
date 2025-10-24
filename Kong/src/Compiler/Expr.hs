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
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import Compiler.Types (CompilerError(..), CompilerEnv)
import qualified Data.Map as M

isKonst :: Type -> Bool
isKonst (TKonst _) = True
isKonst _ = False

compileExpr :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileExpr expr env = case expr of
  AAttribution var rhs ->
    case M.lookup var env of
      Just t | isKonst t -> Left $ IllegalAssignment var
      Just _ ->
        compileExpr rhs env >>= \rhsCode ->
          Right $ rhsCode ++ [PushEnv var, StoreRef]
      Nothing -> Left $ IllegalAssignment ("undeclared variable " ++ var)
  ACall funcName args ->
    fmap (concat . (++ [compileCall funcName])) (mapM (`compileExpr` env) (reverse args))
  AValue astValue -> compileValue astValue env
  AAccess access -> compileAccess access env

compileCall :: String -> [Instr]
compileCall funcName =
  case funcName `elem` builtinOps of
    True -> [DoOp (stringToOp funcName)]
    False -> [PushEnv funcName, Call]

compileValue :: AstValue -> CompilerEnv -> Either CompilerError [Instr]
compileValue val env = case val of
  ANumber (AInteger n) -> Right [Push (VNumber (VInt n))]
  ANumber (AFloat f) -> Right [Push (VNumber (VFloat (realToFrac f)))]
  ANumber (ABool b) -> Right [Push (VNumber (VBool b))]
  ANumber (AChar c) -> Right [Push (VNumber (VChar c))]
  AString s -> Right $ map (Push . VNumber . VChar) s ++ [CreateList (length s)]
  AVarCall vname ->
    case M.lookup vname env of
      Just t | not (isKonst t) -> Right [PushEnv vname, LoadRef]
      _ -> Right [PushEnv vname]
  _ -> Left $ UnsupportedAst ("Unsupported value: " ++ show val)

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess access env =
  case access of
    AArrayAccess arrName idx ->
      compileExpr idx env >>= Right . assemble arrName
    _ -> Left $ UnsupportedAst ("Unsupported access: " ++ show access)
  where
    assemble name idxCode = base name ++ idxCode ++ [GetList]
    base name =
      case M.lookup name env of
        Just t | not (isKonst t) -> [PushEnv name, LoadRef]
        _ -> [PushEnv name]
