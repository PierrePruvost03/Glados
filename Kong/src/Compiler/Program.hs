module Compiler.Program
  ( compileProgram
  , resultsToEither
  , expand
  ) where

import qualified Data.Map as M
import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr)
import Compiler.Types (ProgramError(..), CompilerError(..))
import Compiler.Statements (compileAst)

compileProgram :: [(String, [Ast])] -> Either [ProgramError] [Instr]
compileProgram = resultsToEither . map compilePair . expand

expand :: [(String, [Ast])] -> [(String, Ast)]
expand = concatMap (\(f, as) -> map ((,) f) as)

compilePair :: (String, Ast) -> Either ProgramError [Instr]
compilePair (f, a) = either (Left . ProgramError f a) Right (compile a)

resultsToEither :: [Either ProgramError [Instr]] -> Either [ProgramError] [Instr]
resultsToEither = foldr step (Right [])

step :: Either ProgramError [Instr] -> Either [ProgramError] [Instr] -> Either [ProgramError] [Instr]
step (Left e) (Left es) = Left (e : es)
step (Left e) (Right _) = Left [e]
step (Right _) (Left es) = Left es
step (Right is) (Right js) = Right (is ++ js)

compile :: Ast -> Either CompilerError [Instr]
compile ast = compileAst ast M.empty
