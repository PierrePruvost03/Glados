module Compiler.Expr
  ( compileExpr
  , compileCall
  , compileValue
  , compileAccess
  , inferType
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Syscall (Syscall(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType)
import Compiler.TypeError (prettyTypeError)
import Compiler.Types (checkComparisonTypes, eqTypeNormalized)
import qualified Data.Map as M
import qualified Data.Vector as V

compileExpr :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileExpr (AAttribution var rhs) env =
  case M.lookup var (typeAliases env) of
    Nothing -> Left (UnknownVariable var)
    Just _ -> matchAssignment (M.lookup var (typeAliases env)) (inferType rhs env) (compileExpr rhs env)
  where
    matchAssignment t v (Right rhsCode) =
      case checkAssignmentType t v of
        Right () -> Right (rhsCode ++ [PushEnv var, StoreRef])
        Left err -> Left err
    matchAssignment _ _ (Left err) = Left err
compileExpr (AValue astValue) env = compileValue astValue env
compileExpr (AAccess access) env = compileAccess access env
compileExpr (ACall "print" args) env = compilePrintCall args env
compileExpr (ACall op [lhs, rhs]) env | op `elem` comparisonOps =
  case (inferType lhs env, inferType rhs env) of
    (Just t1, Just t2) ->
      case (compileExpr lhs env, compileExpr rhs env) of
        (Right lcode, Right rcode) ->
          case checkComparisonTypes t1 t2 of
            Right () -> Right (rcode ++ lcode ++ [DoOp (stringToOp op)])
            Left terr -> Left $ InvalidArguments (prettyTypeError terr)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
    _ -> Left $ InvalidArguments "Unable to infer types for comparison"
compileExpr (ACall op [lhs, rhs]) env | op `elem` arithOps =
  case (inferType lhs env, inferType rhs env) of
    (Just t1, Just t2)
      | numericCompatible t1 t2 -> (++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp op)])
      | otherwise -> Left $ InvalidArguments ("Arithmetic operation on incompatible types: " ++ show t1 ++ ", " ++ show t2)
    _ -> Left $ InvalidArguments "Unable to infer types for arithmetic operation"
compileExpr (ACall funcName args) env
  | not (elem funcName (comparisonOps ++ arithOps ++ ["print"])) =
      matchFunctionCall funcName (M.lookup funcName (typeAliases env)) (getFunctionArgTypes (typeAliases env) funcName) (map (\a -> inferType a env) args) (traverse (\a -> compileExpr a env) args)
  | otherwise = fmap (\compiledArgs -> concat compiledArgs ++ compileCall funcName) (mapM (`compileExpr` env) (reverse args))

matchFunctionCall :: String -> Maybe Type -> Maybe [Type] -> [Maybe Type] -> Either CompilerError [[Instr]] -> Either CompilerError [Instr]
matchFunctionCall funcName (Just (TKonst _)) (Just expectedTypes) argTypes (Right argInstrs) =
  case checkFunctionCallTypes expectedTypes argTypes of
    Right () -> Right (concat argInstrs ++ compileCall funcName)
    Left err -> Left err
matchFunctionCall _ (Just (TKonst _)) (Just _) _ (Left err) = Left err
matchFunctionCall funcName (Just (TKonst _)) Nothing _ _ = Left $ UnknownFunction funcName
matchFunctionCall funcName _ _ _ _ = Left $ UnknownFunction funcName

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

checkFunctionCallTypes :: [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes (t:ts) (Just a:as)
  | typesEqual t a || numericCompatible t a = checkFunctionCallTypes ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a)
checkFunctionCallTypes [] [] = Right ()
checkFunctionCallTypes _ _ = Left $ InvalidArguments "Function argument count or type mismatch"

getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just (TKonst (TTuple ts)) -> case ts of
      [] -> Just []
      _  -> Just (init ts)
    _ -> Nothing

getFunctionReturnType :: M.Map String Type -> String -> Maybe Type
getFunctionReturnType envMap fname =
  case M.lookup fname envMap of
    Just (TKonst (TTuple ts)) -> case ts of
      [] -> Nothing
      _  -> Just (last ts)
    _ -> Nothing

stripWrap :: Type -> Type
stripWrap (TKonst t) = stripWrap t
stripWrap (TStrong t) = stripWrap t
stripWrap (TKong t) = stripWrap t
stripWrap t = t

checkAssignmentType :: Maybe Type -> Maybe Type -> Either CompilerError ()
checkAssignmentType (Just expected) (Just actual)
  | eqTypeNormalized expected actual = Right ()
  | otherwise = Left $ IllegalAssignment ("Type mismatch on assignment: expected " ++ show expected ++ ", got " ++ show actual)
checkAssignmentType _ _ = Left $ IllegalAssignment "Unable to infer types for assignment"

typesEqual :: Type -> Type -> Bool
typesEqual = eqTypeNormalized

numericCompatible :: Type -> Type -> Bool
numericCompatible a b =
  case (stripWrap a, stripWrap b) of
    (TInt, TInt) -> True
    (TFloat, TFloat) -> True
    (TInt, TFloat) -> True
    (TFloat, TInt) -> True
    _ -> False

isFloatType :: Type -> Bool
isFloatType t = case stripWrap t of
  TFloat -> True
  _ -> False

isKonst :: Type -> Bool
isKonst (TKonst _) = True
isKonst _ = False

inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType (AValue (ANumber (AInteger _))) _ = Just TInt
inferType (AValue (ANumber (AFloat _))) _ = Just TFloat
inferType (AValue (ANumber (ABool _))) _ = Just TBool
inferType (AValue (ANumber (AChar _))) _ = Just TChar
inferType (AValue (AString _)) _ = Just TString
inferType (AValue (ATuple _)) _ = Nothing
inferType (AValue (AArray _)) _ = Nothing
inferType (AValue (AVector _)) _ = Nothing
inferType (AValue (AStruct _)) _ = Nothing
inferType (AValue (AVarCall v)) env = M.lookup v (typeAliases env)
inferType (AAttribution _ _) _ = Nothing
inferType (AAccess _) _ = Nothing
inferType (ACall fname [l, r]) env | fname `elem` arithOps =
  case (inferType l env, inferType r env) of
    (Just t1, Just t2)
      | numericCompatible t1 t2 && (isFloatType t1 || isFloatType t2) -> Just TFloat
      | numericCompatible t1 t2 -> Just TInt
    _ -> Nothing
inferType (ACall fname _) env
  | fname `elem` (comparisonOps ++ ["print"]) = Nothing
  | otherwise = getFunctionReturnType (typeAliases env) fname

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/"]
