{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Compiler.KongCompiler
  ( compile
  , compileIf
  , compileAst
  , CompilerError(..)
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (Op(..), builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.VM (Env, MemoryCell)
import qualified Data.Map as M

data CompilerError
  = UnsupportedAst String
  | UnknownVariable String
  | UnknownFunction String
  | InvalidArguments String
  deriving (Show, Eq)


getMemoryCell :: Env -> String -> MemoryCell
getMemoryCell env varName =
  case M.lookup varName env of
    Just (_, cell) -> cell
    Nothing -> error $ "Variable not found in environment: " ++ varName


compile :: Ast -> Either CompilerError [Instr]
compile ast = compileAst ast M.empty

compileAst :: Ast -> Env -> Either CompilerError [Instr]
compileAst (ABlock asts) env =
  concat <$> mapM (`compileAst` env) asts
compileAst (AFunkDef name params _ body) env =
  fmap (\instrs -> [Push (VFunction (extractParamNames params) (concat instrs ++ [Ret]) (M.map fst env)), SetVar name])
       (mapM (`compileAst` env) body)
compileAst (AVarDecl varType name Nothing) _ =
  Right [Push (defaultValue varType), SetVar name]
compileAst (AVarDecl _ name (Just value)) env =
  fmap (++ [SetVar name]) (compileExpr value env)
compileAst (AExpress expr) env = compileExpr expr env
compileAst (ASymbol symbol) _ = Right [PushEnv symbol]
compileAst (AReturn expr) env =
  fmap (++ [Ret]) (compileAst expr env)
compileAst ifStmt@(AIf {}) env = compileIf ifStmt env
compileAst ast _ = Left $ UnsupportedAst (show ast)

-- if (x < y)
-- then
  -- zizi
  -- prout
-- 
-- 
-- pushEnv "x"
-- pushEnv "y"
-- DoOp Lt
-- JumpIfFalse  

compileIf :: Ast -> Env -> Either CompilerError [Instr]
compileIf (AIf (AExpress cond) thenBranch@(ABlock thenBlock) [] elseBranch) env =
  concat <$> sequence
  ([ compileExpr cond env
    , Right [JumpIfFalse (length thenBlock)]
    , compileAst thenBranch env
    ] <> f elseBranch)
    where
      f (Just a) = [compileAst a env]
      f Nothing = mempty
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"

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
compileCall funcName
  | funcName `elem` builtinOps = [DoOp (stringToOp funcName)]
  | otherwise                  = [PushEnv funcName, Call]

compileValue :: AstValue -> Env -> Either CompilerError [Instr]
compileValue value _ = case value of
  ANumber (AInteger n) -> Right [Push (VNumber (VInt n))]
  ANumber (AFloat f) -> Right [Push (VNumber (VFloat (realToFrac f)))]
  ANumber (ABool b) -> Right [Push (VNumber (VBool b))]
  ANumber (AChar c) -> Right [Push (VNumber (VChar c))]
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
  TInt -> VNumber $ VInt 0
  TBool -> VNumber $ VBool False
  TChar -> VNumber $ VChar '\0'
  TFloat -> VNumber $ VFloat 0.0
  TString -> VString ""
  TStrong t -> defaultValue t
  TKong t -> defaultValue t
  _ -> VEmpty
