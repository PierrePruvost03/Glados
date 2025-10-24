{-# LANGUAGE LambdaCase #-}

module Compiler.Statements
  ( compileAst
  , compileIf
  , extractParamNames
  , defaultValue
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Number (Number(..))
import qualified Data.Vector as V
import Compiler.Types (CompilerError(..), CompilerEnv)
import Compiler.Expr (compileExpr)
import qualified Data.Map as M

isKonst :: Type -> Bool
isKonst (TKonst _) = True
isKonst _ = False

compileAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileAst ast env =
  case ast of
    ABlock asts ->
      compileBlock asts env
    AFunkDef name params _ body ->
      fmap (registerFunction name params) (compileAst (ABlock body) env)
    AVarDecl t name Nothing ->
      Right (declareDefault t name, M.insert name t env)
    AVarDecl t name (Just initExpr) ->
      fmap (declareWithValue t name) (compileExpr initExpr env)
    AExpress expr ->
      fmap (\instrs -> (instrs, env)) (compileExpr expr env)
    ASymbol symbol ->
      Right (symbolInstrs symbol, env)
    AReturn expr ->
      fmap (\(instrs, _) -> (instrs ++ [Ret], env)) (compileAst expr env)
    a@AIf{} ->
      fmap (\instrs -> (instrs, env)) (compileIf a env)
    _ -> Left $ UnsupportedAst (show ast)
  where
    registerFunction name params (bodyCode, _) =
      ([Push (VFunction (extractParamNames params) (V.fromList (bodyCode ++ [Ret]))), SetVar name], M.insert name (TKonst TInt) env)
    declareDefault t name =
      case isKonst t of
        True -> [Push (defaultValue t), SetVar name]
        False -> [Push (defaultValue t), Alloc, StoreRef, SetVar name]
    declareWithValue t name exprCode =
      ( case isKonst t of
          True -> exprCode ++ [SetVar name]
          False -> exprCode ++ [Alloc, StoreRef, SetVar name]
      , M.insert name t env)
    symbolInstrs symbol =
      case M.lookup symbol env of
        Just t | not (isKonst t) -> [PushEnv symbol, LoadRef]
        _ -> [PushEnv symbol]

compileBlock :: [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
compileBlock asts env =
  foldl (\acc ast ->
    acc >>= \(prevInstrs, currentEnv) ->
      compileAst ast currentEnv >>= \(newInstrs, nextEnv) ->
        Right (prevInstrs ++ newInstrs, nextEnv)
  ) (Right ([], env)) asts

compileIf :: Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf (AIf (AExpress cond) thenBranch elseBranch) env =
  compileExpr cond env >>= \condCode ->
    compileAst thenBranch env >>= \(thenCode, _) ->
      compileElse elseBranch env >>= \(elseCode, _) ->
        Right (assemble condCode thenCode elseCode)
  where
    assemble condCode thenCode elseCode =
      condCode ++ [JumpIfFalse (jumpOffset thenCode elseCode)] ++ thenCode ++ elseSegment elseCode
    jumpOffset thenCode elseCode =
      case elseCode of
        [] -> length thenCode
        _ -> length thenCode + 1
    elseSegment elseCode =
      case elseCode of
        [] -> []
        _ -> Jump (length elseCode) : elseCode
    compileElse maybeBranch scope =
      case maybeBranch of
        Just branch -> compileAst branch scope
        Nothing -> Right ([], scope)
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (ASymbol name) acc = name : acc
    extractParam _ acc = acc

defaultValue :: Type -> Value
defaultValue = \case
  TInt -> VNumber $ VInt 0
  TBool -> VNumber $ VBool False
  TChar -> VNumber $ VChar '\0'
  TFloat -> VNumber $ VFloat 0.0
  TString -> VList (V.fromList []) False
  TStrong t -> defaultValue t
  TKong t -> defaultValue t
  _ -> VEmpty
