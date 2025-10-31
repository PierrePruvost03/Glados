module Compiler.Types
  ( CompilerError(..)
  , ProgramError(..)
  , CompilerEnv (..)
  , emptyEnv
  , insertTypeAlias
  , resolveType
  , checkComparisonTypes
  , eqTypeNormalized
  , checkFunctionCallTypes
  , checkAssignmentType
  , getFunctionArgTypes
  , typesEqual
  , numericCompatible
  , isFloatType
  , isKonst
  , inferType
  , comparisonOps
  , arithOps
  , bothNumeric
  , isLValue
  , isTemporaryValue
  , checkReferenceValidity
  , checkDereferenceValidity
  , validateReferences
  , isRefType
  , extractRefType
  , canInitializeRefWith
  , hasReturn
  , checkFunctionReturn
  , checkReturnType
  , validateReturnsInBody
  , validateReturnsInAst
  , checkMainSignature
  , stripWrap
  , validateStructFieldAccess
  , validateTupleIndexAccess
  , validateAccess
  , validateStructAccess
  , validateStructDefinition
  , validateDivisionByZero
  , validateConstantBounds
  , isValidCast
  , validateKonstAssignment
  , validateArithmeticOperands
  , validateNonCallable
  , validateNoDuplicateDeclaration
  , validateNoDuplicateStruct
  ) where

import DataStruct.Ast
import Parser (LineCount)
import qualified Data.Map as M
import Compiler.TypeError (TypeError(..))
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))

maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall n -> Just n
    _ -> Nothing
  _ -> Nothing

data CompilerEnv = CompilerEnv
  { typeAliases :: M.Map String Type
  , structDefs  :: M.Map String [(Type, String)]
  }

emptyEnv :: CompilerEnv
emptyEnv = CompilerEnv M.empty M.empty

data CompilerError
  = UnsupportedAst String LineCount
  | IllegalAssignment String LineCount
  | UnknownVariable String LineCount
  | UnknownFunction String LineCount
  | InvalidArguments String LineCount
  | MissingMainFunction String
  | InvalidReference String LineCount
  | ReferenceToTemporary String LineCount
  | MissingReturn String LineCount
  | InvalidReturnType String LineCount
  | InvalidMainSignature String LineCount
  | UnknownStructField String LineCount
  | IndexOutOfBounds String LineCount
  | UndefinedStruct String LineCount
  | DivisionByZero String LineCount
  | ConstantOverflow String LineCount
  | InvalidCast String LineCount
  | KonstModification String LineCount
  | NonCallableType String LineCount
  | IncompatibleOperands String LineCount
  | DuplicateDeclaration String LineCount
  | UninitializedVariable String LineCount
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)


insertTypeAlias :: CompilerEnv -> Ast -> CompilerEnv
insertTypeAlias env ast = case unwrap ast of
  ATypeAlias name ty -> env { typeAliases = M.insert name ty (typeAliases env) }
  AStruktDef name fds -> env { structDefs = M.insert name fds (structDefs env) }
  _ -> env

resolveType :: CompilerEnv -> Type -> Type
resolveType env t = case unwrap t of
  TCustom name ->
    case M.lookup name (typeAliases env) of
      Just realTy -> resolveType env realTy
      Nothing -> case M.lookup name (structDefs env) of
        Just _ -> (lc t, TStruct name)
        Nothing -> t
  TKonst ty -> (lc t, TKonst (resolveType env ty))
  TStrong ty -> (lc t, TStrong (resolveType env ty))
  TKong ty -> (lc t, TKong (resolveType env ty))
  TRef ty -> (lc t, TRef (resolveType env ty))
  TStruct s -> (lc t, TStruct s)
  TTrait s -> (lc t, TTrait s)
  TArray ty e -> (lc t, TArray (resolveType env ty) e)
  TVector ty e -> (lc t, TVector (resolveType env ty) e)
  TTuple tys -> (lc t, TTuple (map (resolveType env) tys))
  TFunc args ret -> (lc t, TFunc (map (resolveType env) args) (resolveType env ret))
  raw -> (lc t, raw)

checkComparisonTypes :: Type -> Type -> Either TypeError ()
checkComparisonTypes t1 t2
  | eqTypeNormalized t1 t2 = Right ()
  | bothNumeric (stripWrap t1) (stripWrap t2) = Right ()
  | isNonComparableType (stripWrap t1) || isNonComparableType (stripWrap t2) = 
      Left $ InvalidComparison (show t1 ++ " (non-comparable type)") (show t2)
  | otherwise = Left $ InvalidComparison (show t1) (show t2)

stripWrap :: Type -> Type
stripWrap t = case unwrap t of
  TKonst ty -> stripWrap ty
  TStrong ty -> stripWrap ty
  TKong ty -> stripWrap ty
  _ -> t

bothNumeric :: Type -> Type -> Bool
bothNumeric t1 t2 = case (unwrap t1, unwrap t2) of
  (TInt, TInt) -> True
  (TFloat, TFloat) -> True
  (TInt, TFloat) -> True
  (TFloat, TInt) -> True
  _ -> False

isNonComparableType :: Type -> Bool
isNonComparableType t = case unwrap t of
  TArray _ _ -> True
  TStruct _ -> True
  TTuple _ -> True
  TKonst _ -> True
  _ -> False

eqTypeNormalized :: Type -> Type -> Bool
eqTypeNormalized a b = normalize a == normalize b

normalize :: Type -> Type
normalize t = case unwrap t of
  TKonst ty -> normalize ty
  TStrong ty -> normalize ty
  TKong ty -> normalize ty
  TRef ty -> ((0, 0), TRef (normalize ty))
  TArray ty e -> ((0, 0), TArray (normalize ty) (normalizeExpr e))
  TVector ty e -> ((0, 0), TVector (normalize ty) (normalizeExpr e))
  TTuple tys -> ((0, 0), TTuple (map normalize tys))
  TFunc args ret -> ((0, 0), TFunc (map normalize args) (normalize ret))
  raw -> ((0, 0), raw)

normalizeExpr :: AExpression -> AExpression
normalizeExpr expr = case unwrap expr of
  AValue val -> ((0, 0), AValue (normalizeValue val))
  AAccess acc -> ((0, 0), AAccess (normalizeAccess acc))
  ACall fexp args -> ((0, 0), ACall (normalizeExpr fexp) (map normalizeExpr args))
  ACast ty ex -> ((0, 0), ACast (normalize ty) (normalizeExpr ex))
  AAttribution var rhs -> ((0, 0), AAttribution var (normalizeExpr rhs))
  AMethodCall obj method args -> ((0, 0), AMethodCall (normalizeExpr obj) method (map normalizeExpr args))

normalizeValue :: AstValue -> AstValue
normalizeValue val = case unwrap val of
  ANumber n -> ((0, 0), ANumber n)
  AString s -> ((0, 0), AString s)
  ATuple exprs -> ((0, 0), ATuple (map normalizeExpr exprs))
  AArray exprs -> ((0, 0), AArray (map normalizeExpr exprs))
  AVector exprs -> ((0, 0), AVector (map normalizeExpr exprs))
  AStruct flds -> ((0, 0), AStruct (map (\(n, e) -> (n, normalizeExpr e)) flds))
  ALambda params ret body -> ((0, 0), ALambda params ret body)
  AVarCall v -> ((0, 0), AVarCall v)

normalizeAccess :: AstAccess -> AstAccess
normalizeAccess acc = case unwrap acc of
  AArrayAccess arr idx -> ((0, 0), AArrayAccess (normalizeExpr arr) (normalizeExpr idx))
  AVectorAccess vec idx -> ((0, 0), AVectorAccess (normalizeExpr vec) (normalizeExpr idx))
  ATupleAccess tup idx -> ((0, 0), ATupleAccess (normalizeExpr tup) (normalizeExpr idx))
  AStructAccess str flds -> ((0, 0), AStructAccess (normalizeExpr str) flds)

checkFunctionCallTypes :: LineCount -> [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes lnCount (t:ts) (Just a:as)
  | typesEqual t a || numericCompatible t a = checkFunctionCallTypes lnCount ts as
  | isRefType t && typesEqual (extractRefType t) a = checkFunctionCallTypes lnCount ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a) lnCount
checkFunctionCallTypes lnCount (_:_) (Nothing:_) = Left $ InvalidArguments "Unable to infer argument type" lnCount
checkFunctionCallTypes lnCount [] remaining@(_:_) = Left $ InvalidArguments ("Too many arguments: expected 0 more, got " ++ show (length remaining)) lnCount
checkFunctionCallTypes lnCount remaining@(_:_) [] = Left $ InvalidArguments ("Too few arguments: expected " ++ show (length remaining) ++ " more") lnCount
checkFunctionCallTypes _ [] [] = Right ()

isRefType :: Type -> Bool
isRefType t = case unwrap t of
  TRef _ -> True
  TKonst ty -> isRefType ty
  TStrong ty -> isRefType ty
  TKong ty -> isRefType ty
  _ -> False

extractRefType :: Type -> Type
extractRefType t = case unwrap t of
  TRef ty -> ty
  TKonst ty -> extractRefType ty
  TStrong ty -> extractRefType ty
  TKong ty -> extractRefType ty
  _ -> t

canInitializeRefWith :: CompilerEnv -> Type -> AExpression -> Either CompilerError Bool
canInitializeRefWith env refType expr
  | not (isRefType refType) = Right False
  | otherwise = 
      case checkReferenceValidity expr (lc expr) of
        Left err -> Left err
        Right () -> case inferType expr env of
          Just exprType -> Right (eqTypeNormalized (extractRefType refType) exprType)
          Nothing -> Right False

getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrap t of
      TKonst innerT -> case unwrap innerT of
        TTuple ts -> case ts of
          [] -> Just []
          _ -> Just (init ts)
        TFunc args _ -> Just args
        _ -> Nothing
      TFunc args _ -> Just args
      _ -> Nothing
    _ -> Nothing

getFunctionReturnType :: M.Map String Type -> String -> Maybe Type
getFunctionReturnType envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrap t of
      TKonst innerT -> case unwrap innerT of
        TTuple ts -> case ts of
          [] -> Nothing
          _ -> Just (last ts)
        TFunc _ ret -> Just ret
        _ -> Nothing
      TFunc _ ret -> Just ret
      _ -> Nothing
    _ -> Nothing

checkAssignmentType :: LineCount -> Maybe Type -> Maybe Type -> Either CompilerError ()
checkAssignmentType lnCount (Just expected) (Just actual)
  | eqTypeNormalized expected actual = Right ()
  | isRefType expected && eqTypeNormalized (extractRefType expected) actual = Right ()
  | otherwise = Left $ IllegalAssignment ("Type mismatch on assignment: expected " ++ show expected ++ ", got " ++ show actual) lnCount
checkAssignmentType lnCount _ _ = Left $ IllegalAssignment "Unable to infer types for assignment" lnCount

typesEqual :: Type -> Type -> Bool
typesEqual = eqTypeNormalized

numericCompatible :: Type -> Type -> Bool
numericCompatible a b =
  case (unwrap (stripWrap a), unwrap (stripWrap b)) of
    (t1, t2) | isOperableType t1 && isOperableType t2 -> True
    _ -> False
  where
    isOperableType t = case t of
      TInt -> True
      TFloat -> True
      TChar -> True
      TBool -> True
      _ -> False

isFloatType :: Type -> Bool
isFloatType t = case unwrap (stripWrap t) of
  TFloat -> True
  _ -> False

isKonst :: Type -> Bool
isKonst t = case unwrap t of
  TKonst _ -> True
  _ -> False

inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType expr env = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger _) -> Just (lc expr, TInt)
    ANumber (AFloat _) -> Just (lc expr, TFloat)
    ANumber (ABool _) -> Just (lc expr, TBool)
    ANumber (AChar _) -> Just (lc expr, TChar)
    AString _ -> Just (lc expr, TString)
    ALambda _ retType _ -> Just retType
    ATuple exprs ->
      case traverse (\e -> inferType e env) exprs of
        Just ts -> Just (lc expr, TTuple ts)
        Nothing -> Nothing
    AArray exprs -> inferHomogeneousList (lc expr) TArray exprs env
    AVector exprs -> inferHomogeneousList (lc expr) TVector exprs env
    AStruct _ -> Nothing
    AVarCall v ->
      case M.lookup v (typeAliases env) of
        Just t -> case unwrap t of
          TRef ty -> Just ty
          _ -> Just t
        Nothing -> Nothing
  AAttribution _ _ -> Nothing
  AAccess acc -> inferAccessType acc env
  ACast targetType _ -> Just (resolveType env targetType)
  ACall fexp [l, r] | maybeFuncName fexp `elem` map Just arithOps ->
    case (inferType l env, inferType r env) of
      (Just t1, Just t2)
        | numericCompatible t1 t2 && (isFloatType t1 || isFloatType t2) -> Just (lc expr, TFloat)
        | numericCompatible t1 t2 -> Just (lc expr, TInt)
      _ -> Nothing
  ACall fexp [_l, _r] | maybeFuncName fexp `elem` map Just comparisonOps -> Just (lc expr, TBool)
  ACall fexp _
    | maybeFuncName fexp `elem` map Just (comparisonOps ++ ["print"]) -> Nothing
    | Just name <- maybeFuncName fexp -> getFunctionReturnType (typeAliases env) name
    | otherwise -> Nothing
  AMethodCall _ _ _ -> Nothing

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/", "%"]

inferAccessType :: AstAccess -> CompilerEnv -> Maybe Type
inferAccessType acc env = case unwrap acc of
  AArrayAccess arrExpr _ ->
    case inferType arrExpr env of
      Just t -> case unwrap (stripWrap (resolveType env t)) of
        TArray et _ -> Just et
        TVector et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  AVectorAccess vecExpr _ ->
    case inferType vecExpr env of
      Just t -> case unwrap (stripWrap (resolveType env t)) of
        TVector et _ -> Just et
        TArray et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  ATupleAccess tupleExpr idxExpr ->
    case inferType tupleExpr env of
      Just t -> case unwrap (stripWrap (resolveType env t)) of
        TTuple ts -> tupleIndexType' ts idxExpr
        _ -> Nothing
      Nothing -> Nothing
  AStructAccess structExpr flds ->
    case inferType structExpr env of
      Just t0 -> go t0 flds
      Nothing -> Nothing
    where
      go t [] = Just (resolveType env t)
      go t (f:fs) =
        case unwrap (stripWrap (resolveType env t)) of
          TStruct sname ->
            case M.lookup sname (structDefs env) of
              Just fds ->
                case lookup f (map (\(ty, nm) -> (nm, ty)) fds) of
                  Just fty -> go fty fs
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Nothing

tupleIndexType' :: [Type] -> AExpression -> Maybe Type
tupleIndexType' ts expr = case unwrap expr of
  AValue val -> case unwrap val of
    ANumber (AInteger i)
      | i >= 0 && i < length ts -> Just (ts !! i)
      | otherwise -> Nothing
    _ -> Nothing
  _ -> Nothing

inferHomogeneousList :: LineCount -> (Type -> AExpression -> TypeRaw) -> [AExpression] -> CompilerEnv -> Maybe Type
inferHomogeneousList lnCount ctor exprs env =
  case traverse (\e -> inferType e env) exprs of
    Just [] -> Just (lnCount, ctor (lnCount, TInt) ((lnCount, AValue (lnCount, ANumber (AInteger 0)))))
    Just (t:ts) | all (typesEqual t) ts -> Just (lnCount, ctor t ((lnCount, AValue (lnCount, ANumber (AInteger (length exprs))))))
    _ -> Nothing

isLValue :: AExpression -> Bool
isLValue expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall _ -> True
    _ -> False
  AAccess acc -> case unwrap acc of
    AArrayAccess _ _ -> True
    AVectorAccess _ _ -> True
    ATupleAccess _ _ -> True
    AStructAccess _ _ -> True
  _ -> False

isTemporaryValue :: AExpression -> Bool
isTemporaryValue expr = case unwrap expr of
  ACall _ _ -> True
  ACast _ _ -> True
  AValue val -> case unwrap val of
    ANumber _ -> True
    AString _ -> True
    AStruct _ -> True
    AArray _ -> True
    AVector _ -> True
    ATuple _ -> True
    ALambda _ _ _ -> True
    AVarCall _ -> False
  AAccess _ -> False
  AAttribution _ _ -> False
  AMethodCall _ _ _ -> True

checkReferenceValidity :: AExpression -> LineCount -> Either CompilerError ()
checkReferenceValidity expr lineCount
  | isTemporaryValue expr = 
      Left $ ReferenceToTemporary 
        ("Cannot create reference to temporary value: " ++ show expr) lineCount
  | not (isLValue expr) = 
      Left $ InvalidReference 
        ("Cannot create reference to non-lvalue expression: " ++ show expr) lineCount
  | otherwise = Right ()

checkDereferenceValidity :: AExpression -> Type -> LineCount -> Either CompilerError ()
checkDereferenceValidity _ exprType lineCount = 
  case unwrap (stripWrap exprType) of
    TRef _ -> Right ()
    _ -> Left $ InvalidReference 
           ("Attempting to dereference non-reference type: " ++ show exprType) lineCount

validateReferences :: AExpression -> CompilerEnv -> M.Map String Type -> Either CompilerError ()
validateReferences expr env typeEnv = case unwrap expr of
  ACast targetType innerExpr ->
    validateReferences innerExpr env typeEnv >>= \() ->
      case unwrap (stripWrap (resolveType env targetType)) of
        TRef _ -> checkReferenceValidity innerExpr (lc expr)
        _ -> Right ()
  ACall funcExpr args ->
    validateReferences funcExpr env typeEnv >>= \() ->
      mapM_ (\arg -> validateReferences arg env typeEnv) args >>= \() ->
        case maybeFuncName funcExpr of
          Just fname -> case getFunctionArgTypes typeEnv fname of
            Just argTypes -> validateRefArgs args argTypes (lc expr)
            Nothing -> Right ()
          Nothing -> Right ()
  AAccess acc -> validateAccessReferences acc env typeEnv
  AValue val -> case unwrap val of
    ATuple exprs -> mapM_ (\e -> validateReferences e env typeEnv) exprs
    AArray exprs -> mapM_ (\e -> validateReferences e env typeEnv) exprs
    AVector exprs -> mapM_ (\e -> validateReferences e env typeEnv) exprs
    AStruct structFields -> mapM_ (\(_, e) -> validateReferences e env typeEnv) structFields
    _ -> Right ()
  AAttribution _ rhs -> validateReferences rhs env typeEnv
  AMethodCall obj _ args ->
    validateReferences obj env typeEnv >>= \() ->
      mapM_ (\arg -> validateReferences arg env typeEnv) args

validateRefArgs :: [AExpression] -> [Type] -> LineCount -> Either CompilerError ()
validateRefArgs [] [] _ = Right ()
validateRefArgs (arg:args) (t:ts) lineCount =
  case unwrap (stripWrap t) of
    TRef _ -> checkReferenceValidity arg lineCount >>= \() -> validateRefArgs args ts lineCount
    _ -> validateRefArgs args ts lineCount
validateRefArgs _ _ _ = Right ()

validateAccessReferences :: AstAccess -> CompilerEnv -> M.Map String Type -> Either CompilerError ()
validateAccessReferences acc env typeEnv = case unwrap acc of
  AArrayAccess arrExpr idxExpr ->
    validateReferences arrExpr env typeEnv >>= \() ->
      validateReferences idxExpr env typeEnv
  AVectorAccess vecExpr idxExpr ->
    validateReferences vecExpr env typeEnv >>= \() ->
      validateReferences idxExpr env typeEnv
  ATupleAccess tupExpr idxExpr ->
    validateReferences tupExpr env typeEnv >>= \() ->
      validateReferences idxExpr env typeEnv
  AStructAccess structExpr _ -> 
    validateReferences structExpr env typeEnv

hasReturn :: Ast -> Bool
hasReturn ast = case unwrap ast of
  AReturn _ -> True
  ABlock asts -> any hasReturn asts
  AIf _ thenBranch elseBranch -> 
    case elseBranch of
      Just elseAst -> hasReturn thenBranch && hasReturn elseAst
      Nothing -> False
  ALoop _ _ _ _ -> False
  _ -> False

checkFunctionReturn :: Type -> [Ast] -> LineCount -> Either CompilerError ()
checkFunctionReturn retType body lineCount = 
  case unwrap (stripWrap retType) of
    TInt -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TFloat -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TBool -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TChar -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TString -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TArray _ _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TVector _ _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TTuple _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TStruct _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    TRef _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
    _ -> Right ()

checkReturnType :: AExpression -> Type -> CompilerEnv -> LineCount -> Either CompilerError ()
checkReturnType expr expectedType env lineCount =
  case inferType expr env of
    Just actualType ->
      case eqTypeNormalized expectedType actualType of
        True -> Right ()
        False -> case (bothNumeric expectedType actualType) of
          True -> Right ()
          False -> Left $ InvalidReturnType 
            ("Return type mismatch: expected " ++ show expectedType ++ ", got " ++ show actualType) lineCount
    Nothing -> Right ()

validateReturnsInBody :: [Ast] -> Type -> CompilerEnv -> Either CompilerError ()
validateReturnsInBody asts expectedType env = 
  mapM_ (validateReturnsInAst expectedType env) asts

validateReturnsInAst :: Type -> CompilerEnv -> Ast -> Either CompilerError ()
validateReturnsInAst expectedType env ast = case unwrap ast of
  AReturn returnAst -> case unwrap returnAst of
    AExpress expr -> checkReturnType expr expectedType env (lc ast)
    _ -> Right ()
  ABlock asts -> validateReturnsInBody asts expectedType env
  AIf _ thenBranch elseBranch ->
    validateReturnsInAst expectedType env thenBranch >>= \() ->
      case elseBranch of
        Just elseAst -> validateReturnsInAst expectedType env elseAst
        Nothing -> Right ()
  ALoop _ _ _ body -> validateReturnsInAst expectedType env body
  _ -> Right ()

checkMainSignature :: Type -> LineCount -> Either CompilerError ()
checkMainSignature funcType lineCount = case unwrap funcType of
  TKonst innerT -> case unwrap innerT of
    TFunc [] retType -> case unwrap (stripWrap retType) of
      TInt -> Right ()
      _ -> Left $ InvalidMainSignature "Main function must return Int" lineCount
    _ -> Left $ InvalidMainSignature "Main function must have no parameters and return Int" lineCount
  TFunc [] retType -> case unwrap (stripWrap retType) of
    TInt -> Right ()
    _ -> Left $ InvalidMainSignature "Main function must return Int" lineCount
  _ -> Left $ InvalidMainSignature "Main function must have no parameters and return Int" lineCount

validateStructFieldAccess :: CompilerEnv -> String -> String -> LineCount -> Either CompilerError Type
validateStructFieldAccess env structName fieldName lnCount =
  case M.lookup structName (structDefs env) of
    Nothing -> Left $ UndefinedStruct ("Struct '" ++ structName ++ "' is not defined") lnCount
    Just fds ->
      case lookup fieldName (map (\(ty, nm) -> (nm, ty)) fds) of
        Nothing -> Left $ UnknownStructField 
          ("Field '" ++ fieldName ++ "' does not exist in struct '" ++ structName ++ "'") lnCount
        Just fieldType -> Right fieldType

validateTupleIndexAccess :: [Type] -> AExpression -> LineCount -> Either CompilerError Type
validateTupleIndexAccess ts idxExpr lineCount = case unwrap idxExpr of
  AValue val -> case unwrap val of
    ANumber (AInteger i)
      | i < 0 -> Left $ IndexOutOfBounds 
          ("Tuple index " ++ show i ++ " is negative") lineCount
      | i >= length ts -> Left $ IndexOutOfBounds 
          ("Tuple index " ++ show i ++ " is out of bounds (tuple has " ++ show (length ts) ++ " elements)") lineCount
      | otherwise -> Right (ts !! i)
    _ -> Left $ InvalidArguments "Tuple index must be a constant integer" lineCount
  _ -> Left $ InvalidArguments "Tuple index must be a constant integer" lineCount

validateArrayIndexAccess :: Type -> AExpression -> AExpression -> LineCount -> Either CompilerError ()
validateArrayIndexAccess _ sizeExpr idxExpr lineCount = 
  case (unwrap sizeExpr, unwrap idxExpr) of
    (AValue sizeVal, AValue idxVal) -> 
      case (unwrap sizeVal, unwrap idxVal) of
        (ANumber (AInteger size), ANumber (AInteger idx))
          | idx < 0 -> Left $ IndexOutOfBounds 
              ("Array index " ++ show idx ++ " is negative") lineCount
          | idx >= size -> Left $ IndexOutOfBounds 
              ("Array index " ++ show idx ++ " is out of bounds (array size is " ++ show size ++ ")") lineCount
          | otherwise -> Right ()
        _ -> Right ()
    _ -> Right ()

validateAccess :: AstAccess -> CompilerEnv -> LineCount -> Either CompilerError ()
validateAccess acc env lineCount = case unwrap acc of
  AArrayAccess arrExpr idxExpr -> case inferType arrExpr env of
    Just t -> case unwrap (stripWrap (resolveType env t)) of
      TArray elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lineCount
      TVector elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lineCount
      _ -> Left $ InvalidArguments "Array access on non-array type" lineCount
    Nothing -> Right ()
  AVectorAccess vecExpr idxExpr -> case inferType vecExpr env of
    Just t -> case unwrap (stripWrap (resolveType env t)) of
      TVector elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lineCount
      TArray elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lineCount
      _ -> Left $ InvalidArguments "Vector access on non-vector type" lineCount
    Nothing -> Right ()
  ATupleAccess tupExpr idxExpr -> case inferType tupExpr env of
    Just t -> case unwrap (stripWrap (resolveType env t)) of
      TTuple ts -> validateTupleIndexAccess ts idxExpr lineCount >>= \_ -> Right ()
      _ -> Left $ InvalidArguments "Tuple access on non-tuple type" lineCount
    Nothing -> Right ()
  AStructAccess structExpr fds -> 
    validateStructAccess structExpr fds env lineCount

validateStructAccess :: AExpression -> [String] -> CompilerEnv -> LineCount -> Either CompilerError ()
validateStructAccess expr fds env lineCount = case inferType expr env of
  Just t0 -> go t0 fds
  Nothing -> Right ()
  where
    go _ [] = Right ()
    go t (f:fs) = case unwrap (stripWrap (resolveType env t)) of
      TStruct sname ->
        validateStructFieldAccess env sname f lineCount >>= \fieldType ->
          go fieldType fs
      _ -> Left $ InvalidArguments "Struct access on non-struct type" lineCount

isPrimitiveType :: String -> Bool
isPrimitiveType typeName = typeName `elem` ["Int", "Float", "String", "Char", "Bool", "Void", "Array", "Vector"]

validateStructDefinition :: CompilerEnv -> String -> [(Type, String)] -> LineCount -> Either CompilerError ()
validateStructDefinition env structName fds lineCount = 
  mapM_ validateField fds
  where
    validateField (fieldType, _) = validateTypeExists structName env fieldType lineCount
    
    validateTypeExists :: String -> CompilerEnv -> Type -> LineCount -> Either CompilerError ()
    validateTypeExists sName envir ty lnCount = case unwrap ty of
      TCustom typeName
        | isPrimitiveType typeName -> Right ()
        | otherwise -> case M.lookup typeName (typeAliases envir) of
            Just _ -> Right ()
            Nothing -> case M.lookup typeName (structDefs envir) of
              Just _ -> Right ()
              Nothing -> Left $ UndefinedStruct 
                ("Type '" ++ typeName ++ "' used in struct '" ++ sName ++ "' is not defined") lnCount
      TArray elemType _ -> validateTypeExists sName envir elemType lnCount
      TVector elemType _ -> validateTypeExists sName envir elemType lnCount
      TTuple types -> mapM_ (\t -> validateTypeExists sName envir t lnCount) types
      TKonst innerType -> validateTypeExists sName envir innerType lnCount
      TStrong innerType -> validateTypeExists sName envir innerType lnCount
      TKong innerType -> validateTypeExists sName envir innerType lnCount
      TRef innerType -> validateTypeExists sName envir innerType lnCount
      TStruct sname -> 
        case M.lookup sname (structDefs env) of
          Just _ -> Right ()
          Nothing -> Left $ UndefinedStruct 
            ("Struct '" ++ sname ++ "' used in struct '" ++ sName ++ "' is not defined") lnCount
      _ -> Right ()

validateDivisionByZero :: AExpression -> AExpression -> LineCount -> Either CompilerError ()
validateDivisionByZero _lhs rhs lineCount = case unwrap rhs of
  AValue val -> case unwrap val of
    ANumber (AInteger 0) -> Left $ DivisionByZero "Division by literal zero" lineCount
    ANumber (AFloat 0.0) -> Left $ DivisionByZero "Division by literal zero" lineCount
    _ -> Right ()
  _ -> Right ()

validateConstantBounds :: Type -> AstNumber -> LineCount -> Either CompilerError ()
validateConstantBounds ty num lineCount = 
  case (unwrap ty, num) of
    (TInt, AInteger n) 
      | toInteger n < -2147483648 || toInteger n > 2147483647 -> 
          Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for int") lineCount
    (TStrong innerT, AInteger n) -> case unwrap innerT of
      TInt | toInteger n < -9223372036854775808 || toInteger n > 9223372036854775807 -> 
          Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for long") lineCount
      TKong innerInnerT -> case unwrap innerInnerT of
        TInt | toInteger n < 0 || toInteger n > 18446744073709551615 -> 
            Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for unsigned long") lineCount
        _ -> Right ()
      _ -> Right ()
    (TKong innerT, AInteger n) -> case unwrap innerT of
      TInt | toInteger n < 0 || toInteger n > 4294967295 -> 
          Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for unsigned int") lineCount
      TStrong innerInnerT -> case unwrap innerInnerT of
        TInt | toInteger n < 0 || toInteger n > 18446744073709551615 -> 
            Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for unsigned long") lineCount
        _ -> Right ()
      _ -> Right ()
    _ -> Right ()

isValidCast :: Type -> Type -> CompilerEnv -> LineCount -> Either CompilerError ()
isValidCast sourceType targetType env lineCount =
  case (unwrap (stripWrap (resolveType env sourceType)), unwrap (stripWrap (resolveType env targetType))) of
    (s, t) | isNumericType' s && isNumericType' t -> Right ()
    (s, t) -> Left $ InvalidCast 
      ("Cannot cast from " ++ show s ++ " to " ++ show t) lineCount
  where
    isNumericType' t = case t of
      TInt -> True
      TBool -> True
      TChar -> True
      TFloat -> True
      _ -> False

validateArithmeticOperands :: String -> Type -> Type -> LineCount -> Either CompilerError ()
validateArithmeticOperands op t1 t2 lineCount =
  case (stripWrap t1, stripWrap t2) of
    (type1, type2) | numericCompatible type1 type2 -> Right ()
    (type1, type2) -> Left $ IncompatibleOperands 
      ("Arithmetic operator '" ++ op ++ "' cannot be applied to types " ++ show type1 ++ " and " ++ show type2) lineCount

validateNonCallable :: String -> Type -> LineCount -> Either CompilerError ()
validateNonCallable var t lineCount =
  case unwrap t of
    TKonst _ -> Right ()
    _ -> Left $ NonCallableType 
      ("'" ++ var ++ "' of type " ++ show t ++ " is not callable") lineCount

validateKonstAssignment :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateKonstAssignment var env lineCount = 
  case M.lookup var (typeAliases env) of
    Just t | isKonst t -> Left $ KonstModification 
      ("Cannot modify Konst variable '" ++ var ++ "'") lineCount
    _ -> Right ()

validateNoDuplicateDeclaration :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateNoDuplicateDeclaration name env lineCount =
  case M.lookup name (typeAliases env) of
    Just _ -> Left $ DuplicateDeclaration 
      ("Variable or function '" ++ name ++ "' is already declared in this scope") lineCount
    Nothing -> Right ()

validateNoDuplicateStruct :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateNoDuplicateStruct name env lineCount =
  case M.lookup name (structDefs env) of
    Just _ -> Left $ DuplicateDeclaration 
      ("Struct '" ++ name ++ "' is already declared") lineCount
    Nothing -> Right ()
