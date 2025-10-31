module Compiler.BytecodeGen.Block.Block
  ( compileAstWith
  , declareWithValue
  , compileLoop
  , compileIf
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), resolveType)
import Compiler.Type.Checks (isKonst)
import Compiler.Type.Validation (validateStructDefinition, validateNoDuplicateDeclaration, validateNoDuplicateStruct)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.BytecodeGen.Utils (prebindKonsts)
import Compiler.BytecodeGen.Block.Helpers (validateInitializerValue, compileVarInitialization)
import qualified Data.Map as M

-- Compile an AST node with a custom expression compiler
-- Returns compiled instructions and updated environment
compileAstWith :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
               -> Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAstWith compileExpr ast env = case unwrap ast of
  ABlock asts ->
    foldl
      (\acc a -> acc >>= \(code, sc) ->
          compileAstWith compileExpr a sc >>= \(code', sc') -> Right (code ++ code', sc'))
      (Right ([], prebindKonsts asts env))
      asts
  AVarDecl _ name Nothing ->
    validateNoDuplicateDeclaration name env (lc ast) >>
    Left (UninitializedVariable ("Variable '" ++ name ++ "' must be initialized at declaration") (lc ast))
  AVarDecl t name (Just initExpr) ->
    validateNoDuplicateDeclaration name env (lc ast) >>
    validateInitializerValue t initExpr (lc ast) >>
    compileVarInitialization compileExpr t name initExpr env declareWithValue (lc ast)
  AExpress e ->
    fmap (\code -> (code, env)) (compileExpr e env)
  AReturn a ->
    compileAstWith compileExpr a env >>= \(code, sc) -> Right (code ++ [Ret], sc)
  AStruktDef name fdls ->
    validateNoDuplicateStruct name env (lc ast) >>
    case validateStructDefinition env name fdls (lc ast) of
      Left err -> Left err
      Right () -> Right ([], env { structDefs = M.insert name fdls (structDefs env) })
  ALoop _ _ _ _ ->
    fmap (\instrs -> (instrs, env)) (compileLoop compileExpr ast env)
  AIf _ _ _ ->
    fmap (\instrs -> (instrs, env)) (compileIf compileExpr ast env)
  other -> Left $ UnsupportedAst (show other) (lc ast)

-- Declare a variable with an initialization value
declareWithValue :: CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv)
declareWithValue env t name exprCode
  | isKonst t' = (exprCode ++ [SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  | otherwise = (exprCode ++ [Alloc, StoreRef, SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  where 
    t' = resolveType env t

-- Compile a loop (for/while) into bytecode instructions
compileLoop :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
            -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileLoop compileExpr ast env = case unwrap ast of
  ALoop initAst cond incr body ->
    f initAst env >>= \(compiledInit, newenv) ->
      compileAstWith compileExpr cond newenv >>= \(compiledCond, newenv') ->
        f incr newenv' >>= \(compiledIncr, newenv'') ->
          compileAstWith compileExpr body newenv'' >>= \(compiledBody, _) ->
            Right [JumpIfFalse (length compiledBody + length compiledIncr + 2)] >>= \bodyJump ->
              Right [Jump (- (length compiledBody + length compiledIncr + length compiledCond + length bodyJump + 1))] >>= \jumpBack ->
               Right (compiledInit <> compiledCond <> bodyJump <> compiledBody <> compiledIncr <> jumpBack)
    where
      f (Just a) newEnv = compileAstWith compileExpr a newEnv
      f Nothing newEnv = Right ([], newEnv)
  _ -> Left $ UnsupportedAst "Loop not supported" (lc ast)

-- Compile an if-else statement into bytecode with conditional jumps
compileIf :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
          -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf compileExpr ast env = case unwrap ast of
  AIf condAst thenBranch elseBranch ->
    case unwrap condAst of
      AExpress cond ->
        compileExpr cond env >>= \compiledCond ->
          compileAstWith compileExpr thenBranch env >>= \(compiledThen, _) ->
            Right [JumpIfFalse (length compiledThen + 2)] >>= \condJump ->
              f elseBranch >>= \(compiledElse, _) ->
                Right [Jump (length compiledElse + 1)] >>= \thenJump ->
                  Right (compiledCond <> condJump <> compiledThen <> thenJump <> compiledElse)
        where
          f (Just a) = compileAstWith compileExpr a env
          f Nothing = Right ([], env)
      _ -> Left $ UnsupportedAst "If condition must be an expression" (lc ast)
  _ -> Left $ UnsupportedAst "If statement not supported" (lc ast)
