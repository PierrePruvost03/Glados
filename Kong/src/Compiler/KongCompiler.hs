{-# LANGUAGE LambdaCase #-}

module Compiler.KongCompiler
  ( compile
  , compileAst
  , CompilerError(..)
  ) where

import DataStruct.Ast
import DataStruct.Bytecode (Value(..), Instr(..), Op(..), builtinOps, stringToOp)
import DataStruct.VM (Env)
import qualified Data.Map as M

data CompilerError
  = UnsupportedAst String
  | UnknownVariable String
  | UnknownFunction String
  | InvalidArguments String
  deriving (Show, Eq)

compile :: Ast -> Either CompilerError [Instr]
compile ast = compileAst ast M.empty

compileAst :: Ast -> Env -> Either CompilerError [Instr]
compileAst ast env = case ast of
  ABlock asts -> 
    fmap concat $ mapM (`compileAst` env) asts
  AFunkDef name params returnType body -> 
    fmap (\instrs -> [Push (VFunction (extractParamNames params) (concat instrs ++ [Ret]) env), SetVar name]) 
         (mapM (`compileAst` env) body)
  AVarDecl varType name Nothing -> 
    Right [Push (defaultValue varType), SetVar name]
  AVarDecl varType name (Just value) -> 
    fmap (++ [SetVar name]) (compileExpr value env)
  AExpress expr -> compileExpr expr env
  ASymbol symbol -> Right [PushEnv symbol]
  AReturn expr -> 
    fmap (++ [Ret]) (compileAst expr env)
  _ -> Left $ UnsupportedAst (show ast)

compileExpr :: AExpression -> Env -> Either CompilerError [Instr]
compileExpr expr env = case expr of
  AAttribution var value -> 
    fmap (++ [SetVar var]) (compileExpr value env)
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
compileValue value env = case value of
  ANumber (AInteger n) -> Right [Push (VInt n)]
  ANumber (AFloat f) -> Right [Push (VFloat (realToFrac f))]
  ANumber (ABool b) -> Right [Push (VBool b)]
  ANumber (AChar c) -> Right [Push (VChar c)]
  AString s -> Right [Push (VString s)]
  AVarCall varName -> Right [PushEnv varName]
  _ -> Left $ UnsupportedAst ("Unsupported value: " ++ show value)

compileAccess :: AstAccess -> Env -> Either CompilerError [Instr]
compileAccess access env = case access of
  AArrayAccess varName index -> 
    fmap (([PushEnv varName] ++) . (++ [ArrayGet])) (compileExpr index env)
  _ -> Left $ UnsupportedAst ("Unsupported access: " ++ show access)

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (ASymbol name) acc = name : acc
    extractParam _ acc = acc

defaultValue :: Type -> Value
defaultValue = \case
  TInt -> VInt 0
  TBool -> VBool False
  TChar -> VChar '\0'
  TString -> VString ""
  TFloat -> VFloat 0.0
  TStrong t -> defaultValue t
  TKong t -> defaultValue t
  _ -> VEmpty
