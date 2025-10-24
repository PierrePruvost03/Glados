{-# LANGUAGE LambdaCase #-}

module Compiler.Statements
  ( compileAst
  , compileIf
  , extractParamNames
  , defaultValue
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..), Env)
import DataStruct.Bytecode.Number (Number(..))
import qualified Data.Vector as V
import Compiler.Types (CompilerError(..))
import Compiler.Expr (compileExpr)

compileAst :: Ast -> Env -> Either CompilerError [Instr]
compileAst ast env = case ast of
  ABlock asts ->
    fmap concat $ mapM (`compileAst` env) asts
  AFunkDef name params _ body ->
    fmap (\instrs -> [Push (VFunction (extractParamNames params) (V.fromList (concat instrs ++ [Ret]))), SetVar name])
         (mapM (`compileAst` env) body)
  AVarDecl t name Nothing ->
    Right [Push (defaultValue t), SetVar name]
  AVarDecl _ name (Just initExpr) ->
    fmap (++ [SetVar name]) (compileExpr initExpr env)
  AExpress expr -> compileExpr expr env
  ASymbol symbol -> Right [PushEnv symbol]
  AReturn expr -> fmap (++ [Ret]) (compileAst expr env)
  a@AIf{} -> compileIf a env
  _ -> Left $ UnsupportedAst (show ast)

compileIf :: Ast -> Env -> Either CompilerError [Instr]
compileIf (AIf (AExpress cond) thenBranch elseBranch) env =
  fmap assemble (pair (compileExpr cond env)
                      (pair (compileAst thenBranch env)
                            (maybe (Right []) (`compileAst` env) elseBranch)))
  where
    pair a b = (,) <$> a <*> b
    assemble (condCode, (thenCode, elseCode)) =
      condCode ++ [JumpIfFalse (length thenCode + 1)] ++ thenCode ++ elseCode
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"

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
  TString -> VList (V.fromList []) False
  TStrong t -> defaultValue t
  TKong t -> defaultValue t
  _ -> VEmpty
