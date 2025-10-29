module Compiler.Block
  ( compileAstWith
  , declareDefault
  , declareWithValue
  , defaultValue
  , compileLoop
  , compileIf
  , compileExprForInit
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Number (Number(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType, isKonst, inferType, eqTypeNormalized, bothNumeric, unwrapAst, unwrapType, unwrapExpr, unwrapValue, getAstLineCount)
import qualified Data.Map as M
import Data.Char (isSpace)
import qualified Data.Vector as V
import Data.Int (Int64)
import Data.Word (Word32)

compileAstWith :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
               -> Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAstWith compileExpr ast env = case unwrapAst ast of
  ABlock asts ->
    foldl
      (\acc a -> acc >>= \(code, sc) ->
          compileAstWith compileExpr a sc >>= \(code', sc') -> Right (code ++ code', sc'))
      (Right ([], prebindKonsts asts env))
      asts
  AVarDecl t name Nothing ->
    Right (declareDefault env t name)
  AVarDecl t name (Just initExpr) ->
    case inferType initExpr env of
      Just it
        | typesCompatible env t' it -> fmap (\exprCode -> declareWithValue env' t name exprCode) (compileExprForInit compileExpr initExpr env' (Just t'))
        | otherwise -> Left $ InvalidArguments ("Initializer type mismatch: expected " ++ show t' ++ ", got " ++ show it) (getAstLineCount ast)
        where t' = resolveType env t
              env' = prebindVar t name env
      Nothing -> fmap (\exprCode -> declareWithValue env' t name exprCode) (compileExprForInit compileExpr initExpr env' (Just t'))
        where t' = resolveType env t
              env' = prebindVar t name env
  AExpress e ->
    fmap (\code -> (code, env)) (compileExpr e env)
  AReturn a ->
    compileAstWith compileExpr a env >>= \(code, sc) -> Right (code ++ [Ret], sc)
  AStruktDef name fdls ->
    Right ([], env { structDefs = M.insert name fdls (structDefs env) })
  ALoop _ _ _ _ ->
    fmap (\instrs -> (instrs, env)) (compileLoop compileExpr ast env)
  AIf _ _ _ ->
    fmap (\instrs -> (instrs, env)) (compileIf compileExpr ast env)
  other -> Left $ UnsupportedAst (show other) (getAstLineCount ast)

typesCompatible :: CompilerEnv -> Type -> Type -> Bool
typesCompatible env expected actual =
  eqTypeNormalized expected actual || bothNumeric (strip expected) (strip actual) || arrayAliasMatch
  where
    strip t = case unwrapType t of
      TKonst x -> strip x
      TStrong x -> strip x
      TKong x -> strip x
      _ -> t

    arrayAliasMatch = case (unwrapType (strip expected), unwrapType (strip actual)) of
      (TVector customT sizeExpr, TArray eltT _) ->
        case unwrapType customT of
          TCustom "Array" -> case unwrapExpr sizeExpr of
            AValue val -> case unwrapValue val of
              AVarCall tn -> eqTypeNormalized (resolveType env (fst customT, TCustom tn)) eltT
              _ -> False
            _ -> False
          _ -> False
      _ -> False

declareDefault :: CompilerEnv -> Type -> String -> ([Instr], CompilerEnv)
declareDefault env t name
  | isKonst t' = ([Push (defaultValue t'), SetVar n], env { typeAliases = M.insert n t' (typeAliases env) })
  | otherwise = ([Push (defaultValue t'), Alloc, StoreRef, SetVar n], env { typeAliases = M.insert n t' (typeAliases env) })
  where t' = resolveType env t
        n  = normalizeName name

declareWithValue :: CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv)
declareWithValue env t name exprCode
  | isKonst t' = (exprCode ++ [SetVar n], env { typeAliases = M.insert n t' (typeAliases env) })
  | otherwise = (exprCode ++ [Alloc, StoreRef, SetVar n], env { typeAliases = M.insert n t' (typeAliases env) })
  where t' = resolveType env t
        n  = normalizeName name

defaultValue :: Type -> Value
defaultValue t = case unwrapType t of
  TInt -> VNumber $ VInt 0
  TBool -> VNumber $ VBool False
  TChar -> VNumber $ VChar '\0'
  TFloat -> VNumber $ VFloat 0.0
  TString -> VList (V.fromList [])
  TStrong ty -> defaultValue ty
  TKong ty -> defaultValue ty
  TArray _ _ -> VList (V.fromList [])
  TVector _ _ -> VList (V.fromList [])
  TTuple _ -> VList (V.fromList [])
  _ -> VEmpty

compileLoop :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
            -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileLoop compileExpr ast env = case unwrapAst ast of
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
  _ -> Left $ UnsupportedAst "Loop not supported" (getAstLineCount ast)

compileIf :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
          -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf compileExpr ast env = case unwrapAst ast of
  AIf condAst thenBranch elseBranch ->
    case unwrapAst condAst of
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
      _ -> Left $ UnsupportedAst "If condition must be an expression" (getAstLineCount ast)
  _ -> Left $ UnsupportedAst "If statement not supported" (getAstLineCount ast)

prebindVar :: Type -> String -> CompilerEnv -> CompilerEnv
prebindVar t name env = case unwrapType t of
  TKonst _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

prebindKonsts :: [Ast] -> CompilerEnv -> CompilerEnv
prebindKonsts asts env = foldl step env asts
  where
    step e ast = case unwrapAst ast of
      AVarDecl t n _ | isKonstType t -> e { typeAliases = M.insert n t (typeAliases e) }
      _ -> e
    isKonstType t = case unwrapType t of
      TKonst _ -> True
      _ -> False

normalizeName :: String -> String
normalizeName = reverse . dropWhile isSpace . reverse . dropWhile isSpace

compileNumberWithExpectedType :: AstNumber -> Maybe Type -> Number
compileNumberWithExpectedType (AInteger n) expectedType = compileIntLiteralWithType n expectedType
compileNumberWithExpectedType (ABool b) _ = VBool b
compileNumberWithExpectedType (AChar c) _ = VChar c
compileNumberWithExpectedType (AFloat f) _ = VFloat (realToFrac f)

compileIntLiteralWithType :: Int -> Maybe Type -> Number
compileIntLiteralWithType n (Just t) = 
  case unwrapType t of
    TStrong innerT -> compileIntLiteralForStrong n innerT
    TKong innerT -> compileIntLiteralForKong n innerT
    TKonst innerT -> compileIntLiteralWithType n (Just innerT)
    TInt -> VInt (fromIntegral n)
    _ -> VInt (fromIntegral n)
compileIntLiteralWithType n Nothing = VInt (fromIntegral n)

compileIntLiteralForStrong :: Int -> Type -> Number
compileIntLiteralForStrong n innerT
  | unwrapType innerT == TInt = VLong (fromIntegral n)
  | otherwise = VInt (fromIntegral n)

compileIntLiteralForKong :: Int -> Type -> Number
compileIntLiteralForKong n innerT
  | unwrapType innerT == TInt = VUInt (fromIntegral n)
  | otherwise = VInt (fromIntegral n)

compileExprForInit :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
                   -> AExpression -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr]
compileExprForInit compileExprFn expr env expectedType = 
  case unwrapExpr expr of
    AValue val -> compileValueForInit compileExprFn expr (unwrapValue val) env expectedType
    _ -> compileExprFn expr env

compileValueForInit :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
                    -> AExpression -> AstValueRaw -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr]
compileValueForInit _ _ (ANumber n) _ expectedType = 
  Right [Push (VNumber (compileNumberWithExpectedType n expectedType))]
compileValueForInit compileExprFn expr _ env _ = 
  compileExprFn expr env
