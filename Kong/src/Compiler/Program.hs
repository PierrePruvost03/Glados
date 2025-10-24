module Compiler.Program
  ( compileProgram
  , resultsToEither
  , expand
  , compileWithEnv
  ) where

import qualified Data.Map as M
import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr)
import Compiler.Types (ProgramError(..), CompilerError(..), CompilerEnv, insertTypeAlias)
import Compiler.Statements (compileAst)

compileProgram :: [(String, [Ast])] -> Either [ProgramError] [Instr]
compileProgram fas =
  resultsToEither (map (compilePairWithEnv (buildAliasEnv (concatMap snd fas))) (expand fas))

expand :: [(String, [Ast])] -> [(String, Ast)]
expand pairs = [(file, ast) | (file, asts) <- pairs, ast <- asts]

compilePairWithEnv :: CompilerEnv -> (String, Ast) -> Either ProgramError [Instr]
compilePairWithEnv env (f, a) =
  either (Left . ProgramError f a) Right (compileWithEnv env a)

resultsToEither :: [Either ProgramError [Instr]] -> Either [ProgramError] [Instr]
resultsToEither = foldr step (Right [])

step :: Either ProgramError [Instr] -> Either [ProgramError] [Instr] -> Either [ProgramError] [Instr]
step (Left e) (Left es) = Left (e : es)
step (Left e) (Right _) = Left [e]
step (Right _) (Left es) = Left es
step (Right is) (Right js) = Right (is ++ js)

compileWithEnv :: CompilerEnv -> Ast -> Either CompilerError [Instr]
compileWithEnv env ast = fst <$> compileAst ast env

buildAliasEnv :: [Ast] -> CompilerEnv
buildAliasEnv = foldl insertTypeAlias M.empty
