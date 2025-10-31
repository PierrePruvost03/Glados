module Compiler.Statements
  ( compileAst
  , extractParamNames
  , defaultValue
  , extractGlobalNames
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..))
import Compiler.Type.Validation (validateStructDefinition)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.Expr (compileExpr)
import Compiler.Block (declareDefault, declareWithValue, defaultValue, compileIf)
import qualified Data.Map as M

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst ast env = case unwrap ast of
  ABlock asts -> compileBlock asts env
  AVarDecl t name Nothing ->
    Right (declareDefault env t name)
  AVarDecl t name (Just initExpr) ->
    fmap (declareWithValue (prebindVar t name env) t name)
         (compileExpr initExpr (prebindVar t name env))
  AExpress expr ->
    fmap (\instrs -> (instrs, env)) (compileExpr expr env)
  AReturn expr ->
    fmap (\(instrs, _) -> (instrs ++ [Ret], env)) (compileAst expr env)
  AIf ifC ifT ifE ->
    fmap (\instrs -> (instrs, env)) (compileIf compileExpr (lc ast, AIf ifC ifT ifE) env)
  AStruktDef name fdls ->
    case validateStructDefinition env name fdls (lc ast) of
      Left err -> Left err
      Right () -> Right ([], env { structDefs = M.insert name fdls (structDefs env) })
  AInclude _ _ ->
    Right ([], env)
  raw -> Left $ UnsupportedAst (show raw) (lc ast)

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
    extractParam :: Ast -> [String] -> [String]
    extractParam ast acc = case unwrap ast of
      AVarDecl _ name _ -> name : acc
      _ -> acc

prebindVar :: Type -> String -> CompilerEnv -> CompilerEnv
prebindVar t name env = case unwrap t of
  TKonst _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

prebindKonsts :: [Ast] -> CompilerEnv -> CompilerEnv
prebindKonsts asts env = foldl step env asts
  where
    step :: CompilerEnv -> Ast -> CompilerEnv
    step e ast = case unwrap ast of
      AVarDecl t n _ | isKonstType t -> e { typeAliases = M.insert n t (typeAliases e) }
      _ -> e
    isKonstType :: Type -> Bool
    isKonstType t = case unwrap t of
      TKonst _ -> True
      _ -> False
