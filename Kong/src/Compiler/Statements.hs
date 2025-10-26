module Compiler.Statements
  ( compileAst
  , compileIf
  , extractParamNames
  , defaultValue
  , extractGlobalNames
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Number (Number(..))
import qualified Data.Vector as V
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType, eqTypeNormalized, inferType, isKonst, bothNumeric)
import Compiler.Expr (compileExpr)
import qualified Data.Map as M
import qualified Data.List as L

checkReturnType :: Type -> Maybe Type -> Either CompilerError ()
checkReturnType expected (Just actual)
  | eqTypeNormalized expected actual || (bothNumeric expected actual) = Right ()
  | otherwise = Left $ InvalidArguments ("Return type mismatch: expected " ++ show expected ++ ", got " ++ show actual)
checkReturnType _ Nothing = Left $ InvalidArguments "Unable to infer return type"

getReturnExpr :: [Ast] -> Maybe Ast
getReturnExpr [] = Nothing
getReturnExpr (AReturn e : _) = Just e
getReturnExpr (_ : xs) = getReturnExpr xs

inferReturnType :: Maybe Ast -> CompilerEnv -> Maybe Type
inferReturnType (Just (AExpress e)) env = inferType e env
inferReturnType _ _ = Nothing

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst (ABlock asts) env = compileBlock asts env
-- compileAst (AFunkDef name params retType body) env =
--   matchFunkDef
--   ( compileAst (ABlock body)
--     ( foldl
--       (\e p -> case p of { AVarDecl t n _ -> e { typeAliases = M.insert n t (typeAliases e) }; _ -> e })
--       ( env { typeAliases = M.insert name (TKonst (TTuple (extractParamTypes params ++ [resolveType env retType]))) (typeAliases env) }
--       )
--       params )
--   ) (getReturnExpr body) (resolveType env retType) env name params
compileAst (AVarDecl t name Nothing) env =
  Right (declareDefault env t name)
compileAst (AVarDecl t name (Just initExpr)) env =
  fmap (declareWithValue env t name) (compileExpr initExpr env)
compileAst (AExpress expr) env =
  fmap (\instrs -> (instrs, env)) (compileExpr expr env)
compileAst (AReturn expr) env =
  fmap (\(instrs, _) -> (instrs ++ [Ret], env)) (compileAst expr env)
compileAst aIf@AIf{} env =
  fmap (\instrs -> (instrs, env)) (compileIf aIf env)
compileAst (AStruktDef name fdls) env =
  Right ([], env { structDefs = M.insert name fdls (structDefs env) })
compileAst ast _ = Left $ UnsupportedAst (show ast)

registerFunction :: CompilerEnv -> String -> [Ast] -> ([Instr], CompilerEnv) -> ([Instr], CompilerEnv)
registerFunction env name params (bodyCode, _) =
  ( [Push (VFunction capturedNames (V.fromList (concatMap genParam paramNames ++ bodyCode))), SetVar name]
  , env
  )
  where
    paramNames = extractParamNames params
    globalNames = extractGlobalNames (typeAliases env)
    capturedNames = L.nub (name : paramNames ++ globalNames)
    genParam pname = [Alloc, StoreRef, SetVar pname]

matchFunkDef :: Either CompilerError ([Instr], CompilerEnv) -> Maybe Ast -> Type -> CompilerEnv -> String -> [Ast] -> Either CompilerError ([Instr], CompilerEnv)
matchFunkDef (Right (_bodyInstrs, bodyEnv)) retExpr retType env name params =
  case checkReturnType retType (inferReturnType retExpr bodyEnv) of
    Right () -> Right (registerFunction env name params (_bodyInstrs, bodyEnv))
    Left err -> Left err
matchFunkDef (Left err) _ _ _ _ _ = Left err

extractGlobalNames :: M.Map String a -> [String]
extractGlobalNames = M.keys

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

compileBlock :: [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileBlock asts env =
  foldl (\acc ast ->
    acc >>= \(prevInstrs, currentEnv) ->
      compileAst ast currentEnv >>= \(newInstrs, nextEnv) ->
        Right (prevInstrs ++ newInstrs, nextEnv)
  ) (Right ([], env)) asts

compileIf :: Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf (AIf (AExpress cond) thenBranch elseBranch) env =
  case inferType cond env of
    Just t | eqTypeNormalized t TBool ->
      compileExpr cond env >>= \condCode ->
        compileAst thenBranch env >>= \(thenCode, _) ->
          compileElse elseBranch env >>= \(elseCode, _) ->
            Right (assemble condCode thenCode elseCode)
    Just other -> Left $ InvalidArguments ("If condition must be boolean, got " ++ show other)
    Nothing -> Left $ InvalidArguments "Unable to infer type for if-condition"
  where
    assemble condCode thenCode elseCode =
      condCode ++ [JumpIfFalse (jumpOffset thenCode elseCode)] ++ thenCode ++ elseSegment elseCode
    jumpOffset thenCode [] = length thenCode + 1
    jumpOffset thenCode _ = length thenCode + 2
    elseSegment [] = []
    elseSegment elseCode = Jump (length elseCode) : elseCode
    compileElse Nothing scope = Right ([], scope)
    compileElse (Just branch) scope = compileAst branch scope
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (AVarDecl _ name _) acc = name : acc
    extractParam _ acc = acc

extractParamTypes :: [Ast] -> [Type]
extractParamTypes = foldr go []
  where
    go (AVarDecl t _ _) acc = t : acc
    go _ acc = acc

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
