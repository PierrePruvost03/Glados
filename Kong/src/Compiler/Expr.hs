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
import DataStruct.Bytecode.Syscall (Syscall(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType)
import Compiler.TypeError (TypeError(..), prettyTypeError)
import Compiler.Types (checkComparisonTypes)
import qualified Data.Map as M
import qualified Data.Vector as V

isKonst :: Type -> Bool
isKonst (TKonst _) = True
isKonst _ = False

inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType (AValue (ANumber (AInteger _))) _ = Just TInt
inferType (AValue (ANumber (AFloat _))) _ = Just TFloat
inferType (AValue (ANumber (ABool _))) _ = Just TBool
inferType (AValue (ANumber (AChar _))) _ = Just TChar
inferType (AValue (AString _)) _ = Just TString
inferType (AValue (AVarCall v)) env = M.lookup v (typeAliases env)
inferType (AAttribution _ _) _ = Nothing
inferType (AAccess _) _ = Nothing
inferType (ACall _ _) _ = Nothing

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/"]

compileExpr :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileExpr (AAttribution var rhs) env =
  matchAssignment (M.lookup var (typeAliases env)) (inferType rhs env) (compileExpr rhs env)
  where
    matchAssignment t v (Right rhsCode) =
      case checkAssignmentType t v of
        Right () -> Right (rhsCode ++ [PushEnv var, StoreRef])
        Left err -> Left err
    matchAssignment _ _ (Left err) = Left err
compileExpr (AValue astValue) env = compileValue astValue env
compileExpr (AAccess access) env = compileAccess access env
compileExpr (ACall "print" args) env = compilePrintCall args env
compileExpr (ACall funcName args) env
  | not (elem funcName (comparisonOps ++ arithOps ++ ["print"])) =
      matchFunctionCall (M.lookup funcName (typeAliases env)) (getFunctionArgTypes (typeAliases env) funcName) (map (\a -> inferType a env) args) (traverse (\a -> compileExpr a env) args)
  | otherwise = fmap (concat . (++ [compileCall funcName])) (mapM (`compileExpr` env) (reverse args))

matchFunctionCall :: Maybe Type -> Maybe [Type] -> [Maybe Type] -> Either CompilerError [[Instr]] -> Either CompilerError [Instr]
matchFunctionCall (Just (TKonst _)) (Just expectedTypes) argTypes (Right argInstrs) =
  case checkFunctionCallTypes expectedTypes argTypes of
    Right () -> Right (concat argInstrs ++ [compileCall funcName])
    Left err -> Left err
matchFunctionCall (Just (TKonst _)) (Just _) _ (Left err) = Left err
matchFunctionCall (Just (TKonst _)) Nothing _ _ = Left $ UnknownFunction funcName
matchFunctionCall _ _ _ _ = Left $ UnknownFunction funcName
compileExpr (ACall op [lhs, rhs]) env
  | elem op comparisonOps =
      matchComparison (inferType lhs env) (inferType rhs env) (compileExpr lhs env) (compileExpr rhs env)
  | elem op arithOps =
      matchArith (inferType lhs env) (inferType rhs env) (compileExpr lhs env) (compileExpr rhs env)

matchComparison :: Maybe Type -> Maybe Type -> Either CompilerError [Instr] -> Either CompilerError [Instr] -> Either CompilerError [Instr]
matchComparison (Just t1) (Just t2) (Right lcode) (Right rcode) =
  case checkComparisonTypes t1 t2 of
    Right () -> Right (lcode ++ rcode ++ [DoOp (stringToOp op)])
    Left terr -> Left $ InvalidArguments (prettyTypeError terr)
matchComparison (Just _) (Just _) (Left err) _ = Left err
matchComparison (Just _) (Just _) _ (Left err) = Left err
matchComparison _ _ _ _ = Left $ InvalidArguments "Unable to infer types for comparison"

matchArith :: Maybe Type -> Maybe Type -> Either CompilerError [Instr] -> Either CompilerError [Instr] -> Either CompilerError [Instr]
matchArith (Just t1) (Just t2) (Right lcode) (Right rcode)
  | t1 == t2 && (t1 == TInt || t1 == TFloat) = Right (lcode ++ rcode ++ [DoOp (stringToOp op)])
  | otherwise = Left $ InvalidArguments ("Arithmetic operation on incompatible types: " ++ show t1 ++ ", " ++ show t2)
matchArith (Just _) (Just _) (Left err) _ = Left err
matchArith (Just _) (Just _) _ (Left err) = Left err
matchArith _ _ _ _ = Left $ InvalidArguments "Unable to infer types for arithmetic operation"

compilePrintCall :: [AExpression] -> CompilerEnv -> Either CompilerError [Instr]
compilePrintCall [AValue (AString s)] _ =
  Right [Push (VList (V.fromList (map (VNumber . VChar) s))), Syscall (Print 1)]
compilePrintCall [AValue (ATuple xs)] env =
  compileListLiteral xs env >>= \instrs -> Right (instrs ++ [Syscall (Print 1)])
compilePrintCall [AValue (AArray xs)] env =
  compileListLiteral xs env >>= \instrs -> Right (instrs ++ [Syscall (Print 1)])
compilePrintCall [AValue (AVector xs)] env =
  compileListLiteral xs env >>= \instrs -> Right (instrs ++ [Syscall (Print 1)])
compilePrintCall [arg] env =
  compileExpr arg env >>= \instrs -> Right (instrs ++ [Syscall (Print 1)])
compilePrintCall args env =
  fmap (\instrs -> instrs ++ [Syscall (Print (length args))]) (fmap concat (mapM (`compileExpr` env) (reverse args)))

compileCall :: String -> [Instr]
compileCall "exit" = [Syscall Exit]
compileCall "print" = [Syscall (Print 1)]
compileCall "read" = [Syscall Read]
compileCall "write" = [Syscall Write]
compileCall "open" = [Syscall Open]
compileCall "close" = [Syscall Close]
compileCall name
  | name `elem` builtinOps = [DoOp (stringToOp name)]
  | otherwise = [PushEnv name, Call]

compileValue :: AstValue -> CompilerEnv -> Either CompilerError [Instr]
compileValue (ANumber number) _ = Right [Push (VNumber (compileNumber number))]
compileValue (AString s) _ =
  Right [Push (VList (V.fromList (map (VNumber . VChar) s)))]
compileValue (ATuple exprs) env = compileListLiteral exprs env
compileValue (AArray exprs) env = compileListLiteral exprs env
compileValue (AVector exprs) env = compileListLiteral exprs env
compileValue (AStruct structFields) env = compileStructLiteral structFields env
compileValue (AVarCall vname) env
  | Just t <- M.lookup vname (typeAliases env)
  , not (isKonst (resolveType env t)) = Right [PushEnv vname, LoadRef]
  | otherwise = Right [PushEnv vname]

checkAccessType :: Maybe Type -> Either CompilerError ()
checkAccessType (Just (TArray _ _)) = Right ()
checkAccessType (Just (TVector _ _)) = Right ()
checkAccessType (Just (TStruct _)) = Right ()
checkAccessType (Just _) = Left $ InvalidArguments "Invalid type for access (not a struct/array/vector)"
checkAccessType Nothing = Left $ InvalidArguments "Unable to infer type for access"

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess (AArrayAccess arrName idx) env =
  matchAccess (M.lookup arrName (typeAliases env)) (compileExpr idx env)
compileAccess (AVectorAccess vecName idx) env =
  matchAccess (M.lookup vecName (typeAliases env)) (compileExpr idx env)
compileAccess (ATupleAccess tupleName idx) env =
  matchAccess (M.lookup tupleName (typeAliases env)) (compileExpr idx env)
compileAccess (AStructAccess structName fieldPath) env =
  matchAccess (M.lookup structName (typeAliases env)) (Right (map GetStruct fieldPath))

matchAccess :: Maybe Type -> Either CompilerError [Instr] -> Either CompilerError [Instr]
matchAccess t (Right code) =
  case checkAccessType t of
    Right () -> Right code
    Left err -> Left err
matchAccess _ (Left err) = Left err

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

checkFunctionCallTypes :: [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes (t:ts) (Just a:as)
  | t == a = checkFunctionCallTypes ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a)
checkFunctionCallTypes [] [] = Right ()
checkFunctionCallTypes _ _ = Left $ InvalidArguments "Function argument count or type mismatch"

getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just (TKonst (TTuple ts)) -> Just ts
    _ -> Nothing
