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
compileExpr (AAttribution var rhs) env =
  assignTarget (M.lookup var env)
  where
    assignTarget (Just t)
      | isKonst t = Left $ IllegalAssignment var
    assignTarget (Just _) =
      compileExpr rhs env >>= \rhsCode ->
        Right (rhsCode ++ [PushEnv var, StoreRef])
    assignTarget Nothing = Left $ IllegalAssignment ("undeclared variable " ++ var)
compileExpr (ACall funcName args) env =
  fmap (concat . (++ [compileCall funcName])) (mapM (`compileExpr` env) (reverse args))
compileExpr (AValue astValue) env = compileValue astValue env
compileExpr (AAccess access) env = compileAccess access env

compileCall :: String -> [Instr]
compileCall funcName
  | funcName `elem` builtinOps = [DoOp (stringToOp funcName)]
  | otherwise = [PushEnv funcName, Call]

compileValue :: AstValue -> CompilerEnv -> Either CompilerError [Instr]
compileValue (ANumber number) _ = Right [Push (VNumber (compileNumber number))]
compileValue (AString s) _ = Right (map (Push . VNumber . VChar) s ++ [CreateList (length s)])
compileValue (AVarCall vname) env
  | Just t <- M.lookup vname env
  , not (isKonst t) = Right [PushEnv vname, LoadRef]
  | otherwise = Right [PushEnv vname]
compileValue val _ = Left $ UnsupportedAst ("Unsupported value: " ++ show val)

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess (AArrayAccess arrName idx) env =
  compileExpr idx env >>= Right . assemble arrName
  where
    assemble name idxCode = base name ++ idxCode ++ [GetList]
    base name
      | Just t <- M.lookup name env
      , not (isKonst t) = [PushEnv name, LoadRef]
      | otherwise = [PushEnv name]
compileAccess access _ = Left $ UnsupportedAst ("Unsupported access: " ++ show access)

compileNumber :: AstNumber -> Number
compileNumber (AInteger n) = VInt n
compileNumber (AFloat f) = VFloat (realToFrac f)
compileNumber (ABool b) = VBool b
compileNumber (AChar c) = VChar c
