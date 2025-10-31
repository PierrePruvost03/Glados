module Compiler.BytecodeGen.Program
  ( compileProgram
  , resultsToEither
  , expand
  , compileWithEnv
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..))
import Compiler.Type.Error (ProgramError(..), CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), emptyEnv, insertInEnv)
import Compiler.Type.Return (checkMainSignature)
import Compiler.Type.Validation (validateStructDefinition, validateNoDuplicateDeclaration, validateNoDuplicateStruct)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.BytecodeGen.Statements (compileAst)
import qualified Data.Map as M

compileProgram :: [(String, [Ast])] -> Either [ProgramError] [Instr]
compileProgram fas =
  either Left processWithEnv (buildAliasEnv (concatMap snd fas))
  where
    processWithEnv initialEnv = selectResult allInstrs finalEnv allErrs
      where
        (allInstrs, finalEnv, allErrs) = foldl compileInOrder ([], initialEnv, []) (expand fas)
        compileInOrder (accInstrs, accEnv, accErrs) (file, ast) =
          either
            (\err -> (accInstrs, enrichEnvWithAst accEnv ast, accErrs ++ [err]))
            (\instrs -> (accInstrs ++ instrs, enrichEnvWithAst accEnv ast, accErrs))
            (compilePairWithEnv accEnv (file, ast))
        selectResult instrs env [] = ensureMain instrs env
        selectResult _ _ errs = Left errs

enrichEnvWithAst :: CompilerEnv -> Ast -> CompilerEnv
enrichEnvWithAst env ast = case unwrap ast of
  AVarDecl t name _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

ensureMain :: [Instr] -> CompilerEnv -> Either [ProgramError] [Instr]
ensureMain instrs env = case hasMain instrs of
  False -> Left [ProgramError "<global>" ((0, 0), ABlock []) MissingMainFunction]
  True -> case M.lookup "main" (typeAliases env) of
    Nothing -> Left [ProgramError "<global>" ((0, 0), ABlock []) MissingMainFunction]
    Just mainType -> 
      either (\err -> Left [ProgramError "<global>" ((0, 0), ABlock []) err])
             (\() -> Right (instrs ++ [PushEnv "main", Call, Ret]))
             (checkMainSignature mainType ((0, 0)))

hasMain :: [Instr] -> Bool
hasMain = any isSetMain
  where
    isSetMain (SetVar name) = name == "main"
    isSetMain _ = False

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

buildAliasEnv :: [Ast] -> Either [ProgramError] CompilerEnv
buildAliasEnv asts = 
  (\(env, errs) -> case errs of
    [] -> Right env
    _ -> Left errs)
  (foldl stepAlias (emptyEnv, []) (extractTopLevel asts))
  where
    stepAlias (e, errs) a = case unwrap a of
      ATypeAlias name _ -> 
        either (\err -> (e, errs ++ [ProgramError "<global>" a err]))
               (\() -> (insertInEnv e a, errs))
               (validateNoDuplicateDeclaration name e (lc a))
      AStruktDef name fds -> 
        either (\err -> (e, errs ++ [ProgramError "<global>" a err]))
               (\() -> either (\err -> (e, errs ++ [ProgramError "<global>" a err]))
                              (\() -> (insertInEnv e a, errs))
                              (validateStructDefinition e name fds (lc a)))
               (validateNoDuplicateStruct name e (lc a))
      AVarDecl _ name _ ->
        either (\err -> (e, errs ++ [ProgramError "<global>" a err]))
               (\() -> (e, errs))
               (validateNoDuplicateDeclaration name e (lc a))
      _ -> (e, errs)

extractTopLevel :: [Ast] -> [Ast]
extractTopLevel = concatMap extractFromAst

extractFromAst :: Ast -> [Ast]
extractFromAst a = case unwrap a of
  ABlock innerAsts -> concatMap extractFromAst innerAsts
  _ -> [a]
