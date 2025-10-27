module Compiler.Block
  ( compileAstWith
  , declareDefault
  , declareWithValue
  , defaultValue
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
    Nothing -> Left $ InvalidArguments "Unable to infer type for initializer"
compileAstWith compileExpr (AExpress e) env =
  fmap (\code -> (code, env)) (compileExpr e env)
compileAstWith compileExpr (AReturn a) env =
  compileAstWith compileExpr a env >>= \(code, sc) -> Right (code ++ [Ret], sc)
compileAstWith _ (AStruktDef name fdls) env =
  Right ([], env { structDefs = M.insert name fdls (structDefs env) })
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
