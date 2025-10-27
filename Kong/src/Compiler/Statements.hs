module Compiler.Statements
  ( compileAst
  , compileIf
  , extractParamNames
  , defaultValue
  , extractGlobalNames
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), eqTypeNormalized, inferType)
import Compiler.Expr (compileExpr)
import Compiler.Block (declareDefault, declareWithValue, defaultValue)
import qualified Data.Map as M

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst (ABlock asts) env = compileBlock asts env
compileAst (AVarDecl t name Nothing) env =
  Right (declareDefault env t name)
compileAst (AVarDecl t name (Just initExpr)) env =
  fmap (declareWithValue env t name) (compileExpr initExpr env)
compileAst (AExpress expr) env =
  fmap (\instrs -> (instrs, env)) (compileExpr expr env)
compileAst (AReturn expr) env =
  fmap (\(instrs, _) -> (instrs ++ [Ret], env)) (compileAst expr env)
compileAst aIf@AIf{} env =
  fmap (\instrs -> (instrs, env)) (compileIf aIf env)
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
  ) (Right ([], env)) asts

compileIf :: Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf (AIf (AExpress cond) thenBranch elseBranch) env =
  case inferType cond env of
    Just t | eqTypeNormalized t TBool ->
      compileExpr cond env >>= \condCode ->
        compileAst thenBranch env >>= \(thenCode, _) ->
          compileElse elseBranch env >>= \(elseCode, _) ->
            Right (assemble condCode thenCode elseCode)
    Just other -> Left $ InvalidArguments ("If condition must be boolean, got " ++ show other)
    Nothing -> Left $ InvalidArguments "Unable to infer type for if-condition"
  where
    assemble condCode thenCode elseCode =
      condCode ++ [JumpIfFalse (jumpOffset thenCode elseCode)] ++ thenCode ++ elseSegment elseCode
    jumpOffset thenCode [] = length thenCode + 1
    jumpOffset thenCode _ = length thenCode + 2
    elseSegment [] = []
    elseSegment elseCode = Jump (length elseCode) : elseCode
    compileElse Nothing scope = Right ([], scope)
    compileElse (Just branch) scope = compileAst branch scope
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (AVarDecl _ name _) acc = name : acc
    extractParam _ acc = acc
