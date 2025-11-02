module Compiler.BytecodeGen.Program.Program
  ( compileProgram
  , resultsToEither
  , expand
  , compileWithEnv
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (ProgramError(..))
import Compiler.BytecodeGen.Program.Helpers 
  ( ensureMainExists
  , buildAliasEnvironment
  , enrichEnvironmentWithAst
  , expand
  , compilePairWithEnv
  , resultsToEither
  , compileWithEnv
  )

-- Compile multiple files with ASTs into bytecode instructions
-- Validates environment, compiles each AST in order, and ensures main exists
compileProgram :: [(String, [Ast])] -> Either [ProgramError] [Instr]
compileProgram fas =
  buildAliasEnvironment (concatMap snd fas) >>= \initialEnv ->
  selectResult (foldl (compileInOrder initialEnv) ([], initialEnv, []) (expand fas))
  where
    compileInOrder _ (accInstrs, accEnv, accErrs) (file, ast) =
      either
        (\err -> (accInstrs, enrichEnvironmentWithAst accEnv ast, accErrs ++ [err]))
        (\instrs -> (accInstrs ++ instrs, enrichEnvironmentWithAst accEnv ast, accErrs))
        (compilePairWithEnv accEnv (file, ast))
    selectResult (instrs, env, []) = ensureMainExists instrs env
    selectResult (_, _, errs) = Left errs
