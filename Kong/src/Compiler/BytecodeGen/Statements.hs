module Compiler.BytecodeGen.Statements
  ( compileAst
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..))
import Compiler.Type.Validation (validateStructDefinition)
import Compiler.Type.Normalization (typeToString)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.BytecodeGen.Expr.Expr (compileExpr)
import Compiler.BytecodeGen.Block.Block (declareWithValue, compileIf)
import Compiler.BytecodeGen.Utils (prebindVar)
import qualified Data.Map as M

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst ast env = case unwrap ast of
  ABlock asts -> compileBlock asts env
  AVarDecl _ name Nothing ->
    Left (UninitializedVariable ("Variable '" ++ name ++ "' must be initialized at declaration") (lc ast))
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
  ATraitDef name methods ->
    Right ([], env { traitDefs = M.insert name methods (traitDefs env) })
  ATraitImpl traitName implType methods ->
    compileTraitImpl traitName implType methods env
  AInclude _ _ ->
    Right ([], env)
  raw -> Left $ UnsupportedAst (show raw) (lc ast)

compileBlock :: [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileBlock [] env = Right ([], env)
compileBlock (ast:rest) env = 
  compileAst ast env >>= \(instrs1, env1) ->
    compileBlock rest env1 >>= \(instrs2, env2) ->
      Right (instrs1 ++ instrs2, env2)

compileTraitImpl :: String -> Type -> [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileTraitImpl traitName implType methods env = 
  compileBlock methods 
    (env { traitImpls = M.insert 
             (traitName, typeToString implType) 
             (maybe [] (map (\(n, _, _) -> n)) (M.lookup traitName (traitDefs env))) 
             (traitImpls env) })
