module Compiler.Statements
  ( compileAst
  , extractParamNames
  , defaultValue
  , extractGlobalNames
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..))
import Compiler.Expr (compileExpr)
import Compiler.Block (declareDefault, declareWithValue, defaultValue, compileIf)
import qualified Data.Map as M

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst (ABlock asts) env = compileBlock asts env
compileAst (AVarDecl t name Nothing) env =
  Right (declareDefault env t name)
compileAst (AVarDecl t name (Just initExpr)) env =
  fmap (declareWithValue (prebindVar t name env) t name)
       (compileExpr initExpr (prebindVar t name env))
compileAst (AExpress expr) env =
  fmap (\instrs -> (instrs, env)) (compileExpr expr env)
compileAst (AReturn expr) env =
  fmap (\(instrs, _) -> (instrs ++ [Ret], env)) (compileAst expr env)
compileAst aIf@AIf{} env =
  fmap (\instrs -> (instrs, env)) (compileIf compileExpr aIf env)
compileAst (AStruktDef name fdls) env =
  Right ([], env { structDefs = M.insert name fdls (structDefs env) })
compileAst ast _ = Left $ UnsupportedAst (show ast)

extractGlobalNames :: M.Map String a -> [String]
extractGlobalNames = M.keys

compileBlock :: [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileBlock asts env =
  foldl (\acc ast ->
    acc >>= \(prevInstrs, currentEnv) ->
      compileAst ast currentEnv >>= \(newInstrs, nextEnv) ->
        Right (prevInstrs ++ newInstrs, nextEnv)
  ) (Right ([], prebindKonsts asts env)) asts

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (AVarDecl _ name _) acc = name : acc
    extractParam _ acc = acc

prebindVar :: Type -> String -> CompilerEnv -> CompilerEnv
prebindVar t@(TKonst _) name env = env { typeAliases = M.insert name t (typeAliases env) }
prebindVar _ _ env = env

prebindKonsts :: [Ast] -> CompilerEnv -> CompilerEnv
prebindKonsts asts env = foldl step env asts
  where
    step e (AVarDecl t@(TKonst _) n _) = e { typeAliases = M.insert n t (typeAliases e) }
    step e _ = e
