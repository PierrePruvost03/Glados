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
compileAst (ABlock asts) env = compileBlock asts env
compileAst (AFunkDef name params _ body) env =
  fmap (registerFunction env name params) (compileAst (ABlock body) env)
compileAst (AVarDecl t name Nothing) env =
  Right (declareDefault env t name)
compileAst (AVarDecl t name (Just initExpr)) env =
  fmap (declareWithValue env t name) (compileExpr initExpr env)
compileAst (AExpress expr) env =
  fmap (\instrs -> (instrs, env)) (compileExpr expr env)
compileAst (ASymbol symbol) env =
  Right (symbolInstrs env symbol, env)
compileAst (AReturn expr) env =
  fmap (\(instrs, _) -> (instrs ++ [Ret], env)) (compileAst expr env)
compileAst aIf@AIf{} env =
  fmap (\instrs -> (instrs, env)) (compileIf aIf env)
compileAst ast _ = Left $ UnsupportedAst (show ast)

registerFunction :: CompilerEnv -> String -> [Ast] -> ([Instr], CompilerEnv) -> ([Instr], CompilerEnv)
registerFunction env name params (bodyCode, _) =
  ([Push (VFunction (extractParamNames params) (V.fromList (bodyCode ++ [Ret]))), SetVar name], M.insert name (TKonst TInt) env)

declareDefault :: CompilerEnv -> Type -> String -> ([Instr], CompilerEnv)
declareDefault env t name
  | isKonst t = ([Push (defaultValue t), SetVar name], M.insert name t env)
  | otherwise = ([Push (defaultValue t), Alloc, StoreRef, SetVar name], M.insert name t env)

declareWithValue :: CompilerEnv -> Type -> String -> [Instr] -> ([Instr], CompilerEnv)
declareWithValue env t name exprCode
  | isKonst t = (exprCode ++ [SetVar name], M.insert name t env)
  | otherwise = (exprCode ++ [Alloc, StoreRef, SetVar name], M.insert name t env)

symbolInstrs :: CompilerEnv -> String -> [Instr]
symbolInstrs env symbol
  | Just t <- M.lookup symbol env
  , not (isKonst t) = [PushEnv symbol, LoadRef]
  | otherwise = [PushEnv symbol]

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
    jumpOffset thenCode [] = length thenCode
    jumpOffset thenCode _ = length thenCode + 1
    elseSegment [] = []
    elseSegment elseCode = Jump (length elseCode) : elseCode
    compileElse Nothing scope = Right ([], scope)
    compileElse (Just branch) scope = compileAst branch scope
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (ASymbol name) acc = name : acc
    extractParam _ acc = acc

defaultValue :: Type -> Value
defaultValue TInt = VNumber $ VInt 0
defaultValue TBool = VNumber $ VBool False
defaultValue TChar = VNumber $ VChar '\0'
defaultValue TFloat = VNumber $ VFloat 0.0
defaultValue TString = VList (V.fromList []) False
defaultValue (TStrong t) = defaultValue t
defaultValue (TKong t) = defaultValue t
defaultValue _ = VEmpty
