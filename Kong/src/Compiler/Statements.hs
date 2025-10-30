module Compiler.Statements
  ( compileAst
  , extractParamNames
  , defaultValue
  , extractGlobalNames
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), getAstLineCount, unwrapAst, unwrapType, validateStructDefinition)
import Compiler.Expr (compileExpr)
import Compiler.Block (declareDefault, declareWithValue, defaultValue, compileIf)
import qualified Data.Map as M

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst ast env = case unwrapAst ast of
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
    fmap (\instrs -> (instrs, env)) (compileIf compileExpr (getAstLineCount ast, AIf ifC ifT ifE) env)
  AStruktDef name fdls ->
    case validateStructDefinition env name fdls (getAstLineCount ast) of
      Left err -> Left err
      Right () -> Right ([], env { structDefs = M.insert name fdls (structDefs env) })
  AInclude _ _ ->
    Right ([], env)
  raw -> Left $ UnsupportedAst (show raw) (getAstLineCount ast)

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
    extractParam ast acc = case unwrapAst ast of
      AVarDecl _ name _ -> name : acc
      _ -> acc

prebindVar :: Type -> String -> CompilerEnv -> CompilerEnv
prebindVar t name env = case unwrapType t of
  TKonst _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

prebindKonsts :: [Ast] -> CompilerEnv -> CompilerEnv
prebindKonsts asts env = foldl step env asts
  where
    step e ast = case unwrapAst ast of
      AVarDecl t n _ | isKonstType t -> e { typeAliases = M.insert n t (typeAliases e) }
      _ -> e
    isKonstType t = case unwrapType t of
      TKonst _ -> True
      _ -> False
