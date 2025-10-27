module Compiler.Block
  ( compileAstWith
  , declareDefault
  , declareWithValue
  , defaultValue
  , compileLoop
  , compileIf
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Number (Number(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType, isKonst, inferType, eqTypeNormalized, bothNumeric)
import qualified Data.Map as M
import qualified Data.Vector as V

compileAstWith :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
               -> Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAstWith compileExpr (ABlock asts) env =
  foldl
    (\acc a -> acc >>= \(code, sc) ->
        compileAstWith compileExpr a sc >>= \(code', sc') -> Right (code ++ code', sc'))
    (Right ([], env))
    asts
compileAstWith _ (AVarDecl t name Nothing) env =
  Right (declareDefault env t name)
compileAstWith compileExpr (AVarDecl t name (Just initExpr)) env =
  case inferType initExpr env of
    Just it
      | typesCompatible env t' it -> fmap (\exprCode -> declareWithValue env t name exprCode) (compileExpr initExpr env)
      | otherwise -> Left $ InvalidArguments ("Initializer type mismatch: expected " ++ show t' ++ ", got " ++ show it)
      where t' = resolveType env t
    Nothing -> fmap (\exprCode -> declareWithValue env t name exprCode) (compileExpr initExpr env)
compileAstWith compileExpr (AExpress e) env =
  fmap (\code -> (code, env)) (compileExpr e env)
compileAstWith compileExpr (AReturn a) env =
  compileAstWith compileExpr a env >>= \(code, sc) -> Right (code ++ [Ret], sc)
compileAstWith _ (AStruktDef name fdls) env =
  Right ([], env { structDefs = M.insert name fdls (structDefs env) })
compileAstWith compileExpr (loop@ALoop{}) env =
  fmap (\instrs -> (instrs, env)) (compileLoop compileExpr loop env)
compileAstWith compileExpr (ifStmt@AIf{}) env =
  fmap (\instrs -> (instrs, env)) (compileIf compileExpr ifStmt env)
compileAstWith _ other _ = Left $ UnsupportedAst (show other)

typesCompatible :: CompilerEnv -> Type -> Type -> Bool
typesCompatible env expected actual =
  eqTypeNormalized expected actual || bothNumeric (strip expected) (strip actual) || arrayAliasMatch
  where
    strip (TKonst x) = strip x
    strip (TStrong x) = strip x
    strip (TKong x) = strip x
    strip x = x

    arrayAliasMatch = case (strip expected, strip actual) of
      (TVector (TCustom "Array") (AValue (AVarCall tn)), TArray eltT _) ->
        eqTypeNormalized (resolveType env (TCustom tn)) eltT
      _ -> False

declareDefault :: CompilerEnv -> Type -> String -> ([Instr], CompilerEnv)
declareDefault env t name
  | isKonst t' = ([Push (defaultValue t'), SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  | otherwise = ([Push (defaultValue t'), Alloc, StoreRef, SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  where t' = resolveType env t

declareWithValue :: CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv)
declareWithValue env t name exprCode
  | isKonst t' = (exprCode ++ [SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  | otherwise = (exprCode ++ [Alloc, StoreRef, SetVar name], env { typeAliases = M.insert name t' (typeAliases env) })
  where t' = resolveType env t

defaultValue :: Type -> Value
defaultValue TInt = VNumber $ VInt 0
defaultValue TBool = VNumber $ VBool False
defaultValue TChar = VNumber $ VChar '\0'
defaultValue TFloat = VNumber $ VFloat 0.0
defaultValue TString = VList (V.fromList [])
defaultValue (TStrong t) = defaultValue t
defaultValue (TKong t) = defaultValue t
defaultValue (TArray _ _) = VList (V.fromList [])
defaultValue (TVector _ _) = VList (V.fromList [])
defaultValue (TTuple _) = VList (V.fromList [])
defaultValue _ = VEmpty

compileLoop :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
            -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileLoop compileExpr (ALoop initAst cond incr body) env =
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
compileLoop _ _ _ = Left $ UnsupportedAst "Loop not supported"

compileIf :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
          -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf compileExpr (AIf (AExpress cond) thenBranch elseBranch) env =
  compileExpr cond env >>= \compiledCond ->
    compileAstWith compileExpr thenBranch env >>= \(compiledThen, _) ->
      Right [JumpIfFalse (length compiledThen + 2)] >>= \condJump ->
        f elseBranch >>= \(compiledElse, _) ->
          Right [Jump (length compiledElse + 1)] >>= \thenJump ->
            Right (compiledCond <> condJump <> compiledThen <> thenJump <> compiledElse)
  where
    f (Just a) = compileAstWith compileExpr a env
    f Nothing = Right ([], env)
compileIf _ _ _ = Left $ UnsupportedAst "If statement not supported"
