module Compiler.Expr
  ( compileExpr
  , compileCall
  , compileValue
  , compileAccess
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType)
import qualified Data.Map as M
import qualified Data.Vector as V

isKonst :: Type -> Bool
isKonst (TKonst _) = True
isKonst _ = False

compileExpr :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileExpr (AAttribution var rhs) env =
  assignTarget (M.lookup var (typeAliases env))
  where
    assignTarget (Just t)
      | isKonst (resolveType env t) = Left $ IllegalAssignment var
    assignTarget (Just _) =
      compileExpr rhs env >>= \rhsCode ->
        Right (rhsCode ++ [PushEnv var, StoreRef])
    assignTarget Nothing = Left $ IllegalAssignment ("undeclared variable " ++ var)
compileExpr (ACall funcName args) env =
  fmap (concat . (++ [compileCall funcName])) (mapM (`compileExpr` env) (reverse args))
compileExpr (AValue astValue) env = compileValue astValue env
compileExpr (AAccess access) env = compileAccess access env

compileCall :: String -> [Instr]
compileCall funcName
  | funcName `elem` builtinOps = [DoOp (stringToOp funcName)]
  | otherwise = [PushEnv funcName, Call]

compileValue :: AstValue -> CompilerEnv -> Either CompilerError [Instr]
compileValue (ANumber number) _ = Right [Push (VNumber (compileNumber number))]
compileValue (AString s) _ =
  Right [Push (VList (V.fromList (map (VNumber . VChar) s)) False)]
compileValue (ATuple exprs) env = compileListLiteral exprs env
compileValue (AArray exprs) env = compileListLiteral exprs env
compileValue (AVector exprs) env = compileListLiteral exprs env
compileValue (AStruct structFields) env = compileStructLiteral structFields env
compileValue (AVarCall vname) env
  | Just t <- M.lookup vname (typeAliases env)
  , not (isKonst (resolveType env t)) = Right [PushEnv vname, LoadRef]
  | otherwise = Right [PushEnv vname]

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess (AArrayAccess arrName idx) env =
  compileIndexedAccess arrName idx env
compileAccess (AVectorAccess vecName idx) env =
  compileIndexedAccess vecName idx env
compileAccess (ATupleAccess tupleName idx) env =
  compileIndexedAccess tupleName idx env
compileAccess (AStructAccess structName fieldPath) env =
  Right (base structName ++ map GetStruct fieldPath)
  where
    base name
      | Just t <- M.lookup name (typeAliases env)
      , not (isKonst t) = [PushEnv name, LoadRef]
      | otherwise = [PushEnv name]

compileNumber :: AstNumber -> Number
compileNumber (AInteger n) = VInt n
compileNumber (AFloat f) = VFloat (realToFrac f)
compileNumber (ABool b) = VBool b
compileNumber (AChar c) = VChar c

compileListLiteral :: [AExpression] -> CompilerEnv -> Either CompilerError [Instr]
compileListLiteral exprs env =
  fmap (\compiled -> concat compiled ++ [CreateList (length exprs)])
       (mapM (`compileExpr` env) exprs)

compileStructLiteral :: [(String, AExpression)] -> CompilerEnv -> Either CompilerError [Instr]
compileStructLiteral fieldPairs env =
  fmap (\compiled -> concat compiled ++ [CreateStruct fieldNames])
       (mapM compileField (reverse fieldPairs))
  where
    compileField (_, expression) = compileExpr expression env
    fieldNames = map fst fieldPairs

compileIndexedAccess :: String -> AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileIndexedAccess name idx env =
  compileExpr idx env >>= Right . assemble
  where
    assemble idxCode = base name ++ idxCode ++ [GetList]
    base targetName
      | Just t <- M.lookup targetName (typeAliases env)
      , not (isKonst t) = [PushEnv targetName, LoadRef]
      | otherwise = [PushEnv targetName]
