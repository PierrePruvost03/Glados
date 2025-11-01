module Compiler.BytecodeGen.Block.Block
  ( compileAst
  , compileBlock
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), inferType)
import Compiler.Type.Validation (validateStructDefinition, validateNoDuplicateDeclaration, checkAssignmentType)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.BytecodeGen.Expr.Expr (compileExpr, compileIf, compileLoop)
import Compiler.BytecodeGen.Block.Helpers (validateInitializerValue, compileVarInitialization, declareWithValue)
import Compiler.BytecodeGen.Utils (prebindVar)
import qualified Data.Map as M

-- Compile AST with default expression compiler
compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst ast env = case unwrap ast of
  ABlock asts -> compileBlock asts env
  AVarDecl _ name Nothing ->
    validateNoDuplicateDeclaration name env (lc ast) >>
    Left (UninitializedVariable ("Variable '" ++ name ++ "' must be initialized at declaration") (lc ast))
  AVarDecl t name (Just initExpr) ->
    validateNoDuplicateDeclaration name env (lc ast) >>
    validateInitializerValue t initExpr (lc ast) >>
    (case inferType initExpr (prebindVar t name env) of
      Just inferredType -> checkAssignmentType (lc ast) (Just t) (Just inferredType)
      Nothing -> Right ()) >>
    compileVarInitialization compileExpr t name initExpr env declareWithValue (lc ast)
  AExpress expr ->
    fmap (\instrs -> (instrs, env)) (compileExpr expr env)
  AReturn expr ->
    compileAst expr env >>= \(instrs, _) -> 
      Right (instrs ++ [Ret], env)
  AIf _ _ _ ->
    fmap (\instrs -> (instrs, env)) (compileIf compileExpr compileAst ast env)
  ALoop _ _ _ _ ->
    fmap (\instrs -> (instrs, env)) (compileLoop compileExpr compileAst ast env)
  AStruktDef name fdls ->
    case validateStructDefinition env name fdls (lc ast) of
      Left err -> Left err
      Right () -> Right ([], env { structDefs = M.insert name fdls (structDefs env) })
  AInclude _ _ ->
    Right ([], env)
  raw -> Left $ UnsupportedAst (show raw) (lc ast)

-- Compile a block of statements
compileBlock :: [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileBlock asts env =
  foldl
    (\acc a -> acc >>= \(code, sc) ->
        compileAst a sc >>= \(code', sc') -> 
          Right (code ++ code', sc'))
    (Right ([], env))
    asts
