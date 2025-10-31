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
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType, isKonst, inferType, eqTypeNormalized, bothNumeric, isRefType, canInitializeRefWith, validateStructDefinition, validateConstantBounds, validateNoDuplicateDeclaration, validateNoDuplicateStruct)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import qualified Data.Map as M
import Data.Char (isSpace)
import qualified Data.Vector as V

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
    (case unwrap initExpr of
      AValue val -> case unwrap val of
        ANumber num -> validateConstantBounds t num (lc ast)
        _ -> Right ()
      _ -> Right ()) >>
    case inferType initExpr env of
      Just it
        | isRefType t' -> 
            case canInitializeRefWith env t' initExpr of
              Right True -> fmap (\exprCode -> declareWithValue env' t name exprCode) (compileExpr initExpr env')
              Right False -> Left $ InvalidArguments ("Cannot initialize reference of type " ++ show t' ++ " with expression of type " ++ show it) (lc ast)
              Left err -> Left err
        | typesCompatible env t' it -> fmap (\exprCode -> declareWithValue env' t name exprCode) (compileExpr initExpr env')
        | otherwise -> Left $ InvalidArguments ("Initializer type mismatch: expected " ++ show t' ++ ", got " ++ show it) (lc ast)
        where t' = resolveType env t
              env' = prebindVar t name env
      Nothing -> fmap (\exprCode -> declareWithValue env' t name exprCode) (compileExpr initExpr env')
        where env' = prebindVar t name env
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

typesCompatible :: CompilerEnv -> Type -> Type -> Bool
typesCompatible env expected actual =
  eqTypeNormalized expected actual || bothNumeric (strip expected) (strip actual) || arrayAliasMatch
  where
    strip t = case unwrap t of
      TKonst x -> strip x
      TStrong x -> strip x
      TKong x -> strip x
      _ -> t

    arrayAliasMatch = case (unwrap (strip expected), unwrap (strip actual)) of
      (TVector customT sizeExpr, TArray eltT _) ->
        case unwrap customT of
          TCustom "Array" -> case unwrap sizeExpr of
            AValue val -> case unwrap val of
              AVarCall tn -> eqTypeNormalized (resolveType env (fst customT, TCustom tn)) eltT
              _ -> False
            _ -> False
          _ -> False
      _ -> False

declareDefault :: CompilerEnv -> Type -> String -> ([Instr], CompilerEnv)
declareDefault env t name = case unwrap t' of
  TArray _ sizeExpr -> 
    case extractArraySize sizeExpr of
      Just size -> 
        (replicate size (Push VEmpty) ++ [CreateList size, Alloc, StoreRef, SetVar n], 
         env { typeAliases = M.insert n t' (typeAliases env) })
      Nothing -> 
        ([Push (defaultValue t'), Alloc, StoreRef, SetVar n], 
         env { typeAliases = M.insert n t' (typeAliases env) })
  TVector _ sizeExpr -> 
    case extractArraySize sizeExpr of
      Just size -> 
        (replicate size (Push VEmpty) ++ [CreateList size, Alloc, StoreRef, SetVar n], 
         env { typeAliases = M.insert n t' (typeAliases env) })
      Nothing -> 
        ([Push (defaultValue t'), Alloc, StoreRef, SetVar n], 
         env { typeAliases = M.insert n t' (typeAliases env) })
  _ | isKonst t' -> 
      ([Push (defaultValue t'), SetVar n], 
       env { typeAliases = M.insert n t' (typeAliases env) })
    | otherwise -> 
      ([Push (defaultValue t'), Alloc, StoreRef, SetVar n], 
       env { typeAliases = M.insert n t' (typeAliases env) })
  where t' = resolveType env t
        n  = normalizeName name

declareWithValue :: CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv)
declareWithValue env t name exprCode
  | isKonst t' = (exprCode ++ [SetVar n], env { typeAliases = M.insert n t' (typeAliases env) })
  | otherwise = (exprCode ++ [Alloc, StoreRef, SetVar n], env { typeAliases = M.insert n t' (typeAliases env) })
  where t' = resolveType env t
        n  = normalizeName name

defaultValue :: Type -> Value
defaultValue t = case unwrap t of
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

prebindVar :: Type -> String -> CompilerEnv -> CompilerEnv
prebindVar t name env = case unwrap t of
  TKonst _ -> env { typeAliases = M.insert name t (typeAliases env) }
  _ -> env

prebindKonsts :: [Ast] -> CompilerEnv -> CompilerEnv
prebindKonsts asts env = foldl step env asts
  where
    step :: CompilerEnv -> Ast -> CompilerEnv
    step e ast = case unwrap ast of
      AVarDecl t n _ | isKonstType t -> e { typeAliases = M.insert n t (typeAliases e) }
      _ -> e
    isKonstType :: Type -> Bool
    isKonstType t = case unwrap t of
      TKonst _ -> True
      _ -> False

normalizeName :: String -> String
normalizeName = reverse . dropWhile isSpace . reverse . dropWhile isSpace

extractArraySize :: AExpression -> Maybe Int
extractArraySize expr = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger n) -> Just n
    _ -> Nothing
  _ -> Nothing
