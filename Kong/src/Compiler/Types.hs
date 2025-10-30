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
  , unwrapAst
  , unwrapExpr
  , unwrapType
  , unwrapAccess
  , unwrapValue
  , getAstLineCount
  , getExprLineCount
  , getTypeLineCount
  , getAccessLineCount
  , getValueLineCount
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
  ) where

import DataStruct.Ast
import Parser (LineCount)
import qualified Data.Map as M
import Compiler.TypeError (TypeError(..))

maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
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
  deriving (Show, Eq)

data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)


insertTypeAlias :: CompilerEnv -> Ast -> CompilerEnv
insertTypeAlias env ast = case unwrapAst ast of
  ATypeAlias name ty -> env { typeAliases = M.insert name ty (typeAliases env) }
  AStruktDef name fds -> env { structDefs = M.insert name fds (structDefs env) }
  _ -> env

resolveType :: CompilerEnv -> Type -> Type
resolveType env t = case unwrapType t of
  TCustom name ->
    case M.lookup name (typeAliases env) of
      Just realTy -> resolveType env realTy
      Nothing -> case M.lookup name (structDefs env) of
        Just _ -> (fst t, TStruct name)
        Nothing -> t
  TKonst ty -> (fst t, TKonst (resolveType env ty))
  TStrong ty -> (fst t, TStrong (resolveType env ty))
  TKong ty -> (fst t, TKong (resolveType env ty))
  TRef ty -> (fst t, TRef (resolveType env ty))
  TStruct s -> (fst t, TStruct s)
  TTrait s -> (fst t, TTrait s)
  TArray ty e -> (fst t, TArray (resolveType env ty) e)
  TVector ty e -> (fst t, TVector (resolveType env ty) e)
  TTuple tys -> (fst t, TTuple (map (resolveType env) tys))
  TFunc args ret -> (fst t, TFunc (map (resolveType env) args) (resolveType env ret))
  raw -> (fst t, raw)

checkComparisonTypes :: Type -> Type -> Either TypeError ()
checkComparisonTypes t1 t2
  | eqTypeNormalized t1 t2 = Right ()
  | bothNumeric (stripWrap t1) (stripWrap t2) = Right ()
  | isNonComparableType (stripWrap t1) || isNonComparableType (stripWrap t2) = 
      Left $ InvalidComparison (show t1 ++ " (non-comparable type)") (show t2)
  | otherwise = Left $ InvalidComparison (show t1) (show t2)

stripWrap :: Type -> Type
stripWrap t = case unwrapType t of
  TKonst ty -> stripWrap ty
  TStrong ty -> stripWrap ty
  TKong ty -> stripWrap ty
  _ -> t

bothNumeric :: Type -> Type -> Bool
bothNumeric t1 t2 = case (unwrapType t1, unwrapType t2) of
  (TInt, TInt) -> True
  (TFloat, TFloat) -> True
  (TInt, TFloat) -> True
  (TFloat, TInt) -> True
  _ -> False

isNonComparableType :: Type -> Bool
isNonComparableType t = case unwrapType t of
  TArray _ _ -> True
  TStruct _ -> True
  TTuple _ -> True
  TKonst _ -> True
  _ -> False

eqTypeNormalized :: Type -> Type -> Bool
eqTypeNormalized a b = normalize a == normalize b

normalize :: Type -> Type
normalize t = case unwrapType t of
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
normalizeExpr expr = case unwrapExpr expr of
  AValue val -> ((0, 0), AValue (normalizeValue val))
  AAccess acc -> ((0, 0), AAccess (normalizeAccess acc))
  ACall fexp args -> ((0, 0), ACall (normalizeExpr fexp) (map normalizeExpr args))
  ACast ty ex -> ((0, 0), ACast (normalize ty) (normalizeExpr ex))
  AAttribution var rhs -> ((0, 0), AAttribution var (normalizeExpr rhs))
  AMethodCall obj method args -> ((0, 0), AMethodCall (normalizeExpr obj) method (map normalizeExpr args))

normalizeValue :: AstValue -> AstValue
normalizeValue val = case unwrapValue val of
  ANumber n -> ((0, 0), ANumber n)
  AString s -> ((0, 0), AString s)
  ATuple exprs -> ((0, 0), ATuple (map normalizeExpr exprs))
  AArray exprs -> ((0, 0), AArray (map normalizeExpr exprs))
  AVector exprs -> ((0, 0), AVector (map normalizeExpr exprs))
  AStruct flds -> ((0, 0), AStruct (map (\(n, e) -> (n, normalizeExpr e)) flds))
  ALambda params ret body -> ((0, 0), ALambda params ret body)
  AVarCall v -> ((0, 0), AVarCall v)

normalizeAccess :: AstAccess -> AstAccess
normalizeAccess acc = case unwrapAccess acc of
  AArrayAccess arr idx -> ((0, 0), AArrayAccess (normalizeExpr arr) (normalizeExpr idx))
  AVectorAccess vec idx -> ((0, 0), AVectorAccess (normalizeExpr vec) (normalizeExpr idx))
  ATupleAccess tup idx -> ((0, 0), ATupleAccess (normalizeExpr tup) (normalizeExpr idx))
  AStructAccess str flds -> ((0, 0), AStructAccess (normalizeExpr str) flds)

checkFunctionCallTypes :: LineCount -> [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes lc (t:ts) (Just a:as)
  | typesEqual t a || numericCompatible t a = checkFunctionCallTypes lc ts as
  | isRefType t && typesEqual (extractRefType t) a = checkFunctionCallTypes lc ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a) lc
checkFunctionCallTypes lc (_:_) (Nothing:_) = Left $ InvalidArguments "Unable to infer argument type" lc
checkFunctionCallTypes lc [] remaining@(_:_) = Left $ InvalidArguments ("Too many arguments: expected 0 more, got " ++ show (length remaining)) lc
checkFunctionCallTypes lc remaining@(_:_) [] = Left $ InvalidArguments ("Too few arguments: expected " ++ show (length remaining) ++ " more") lc
checkFunctionCallTypes _ [] [] = Right ()

isRefType :: Type -> Bool
isRefType t = case unwrapType t of
  TRef _ -> True
  TKonst ty -> isRefType ty
  TStrong ty -> isRefType ty
  TKong ty -> isRefType ty
  _ -> False

extractRefType :: Type -> Type
extractRefType t = case unwrapType t of
  TRef ty -> ty
  TKonst ty -> extractRefType ty
  TStrong ty -> extractRefType ty
  TKong ty -> extractRefType ty
  _ -> t

canInitializeRefWith :: CompilerEnv -> Type -> AExpression -> Either CompilerError Bool
canInitializeRefWith env refType expr
  | not (isRefType refType) = Right False
  | otherwise = 
      case checkReferenceValidity expr (getExprLineCount expr) of
        Left err -> Left err
        Right () -> case inferType expr env of
          Just exprType -> Right (eqTypeNormalized (extractRefType refType) exprType)
          Nothing -> Right False

getFunctionArgTypes :: M.Map String Type -> String -> Maybe [Type]
getFunctionArgTypes envMap fname =
  case M.lookup fname envMap of
    Just t -> case unwrapType t of
      TKonst innerT -> case unwrapType innerT of
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
    Just t -> case unwrapType t of
      TKonst innerT -> case unwrapType innerT of
        TTuple ts -> case ts of
          [] -> Nothing
          _ -> Just (last ts)
        TFunc _ ret -> Just ret
        _ -> Nothing
      TFunc _ ret -> Just ret
      _ -> Nothing
    _ -> Nothing

checkAssignmentType :: LineCount -> Maybe Type -> Maybe Type -> Either CompilerError ()
checkAssignmentType lc (Just expected) (Just actual)
  | eqTypeNormalized expected actual = Right ()
  | isRefType expected && eqTypeNormalized (extractRefType expected) actual = Right ()
  | otherwise = Left $ IllegalAssignment ("Type mismatch on assignment: expected " ++ show expected ++ ", got " ++ show actual) lc
checkAssignmentType lc _ _ = Left $ IllegalAssignment "Unable to infer types for assignment" lc

typesEqual :: Type -> Type -> Bool
typesEqual = eqTypeNormalized

numericCompatible :: Type -> Type -> Bool
numericCompatible a b =
  case (unwrapType (stripWrap a), unwrapType (stripWrap b)) of
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
isFloatType t = case unwrapType (stripWrap t) of
  TFloat -> True
  _ -> False

isKonst :: Type -> Bool
isKonst t = case unwrapType t of
  TKonst _ -> True
  _ -> False

inferType :: AExpression -> CompilerEnv -> Maybe Type
inferType expr env = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    ANumber (AInteger _) -> Just (getExprLineCount expr, TInt)
    ANumber (AFloat _) -> Just (getExprLineCount expr, TFloat)
    ANumber (ABool _) -> Just (getExprLineCount expr, TBool)
    ANumber (AChar _) -> Just (getExprLineCount expr, TChar)
    AString _ -> Just (getExprLineCount expr, TString)
    ALambda _ retType _ -> Just retType
    ATuple exprs ->
      case traverse (\e -> inferType e env) exprs of
        Just ts -> Just (getExprLineCount expr, TTuple ts)
        Nothing -> Nothing
    AArray exprs -> inferHomogeneousList (getExprLineCount expr) TArray exprs env
    AVector exprs -> inferHomogeneousList (getExprLineCount expr) TVector exprs env
    AStruct _ -> Nothing
    AVarCall v ->
      case M.lookup v (typeAliases env) of
        Just t -> case unwrapType t of
          TRef ty -> Just ty
          _ -> Just t
        Nothing -> Nothing
  AAttribution _ _ -> Nothing
  AAccess acc -> inferAccessType acc env
  ACast targetType _ -> Just (resolveType env targetType)
  ACall fexp [l, r] | maybeFuncName fexp `elem` map Just arithOps ->
    case (inferType l env, inferType r env) of
      (Just t1, Just t2)
        | numericCompatible t1 t2 && (isFloatType t1 || isFloatType t2) -> Just (getExprLineCount expr, TFloat)
        | numericCompatible t1 t2 -> Just (getExprLineCount expr, TInt)
      _ -> Nothing
  ACall fexp [_l, _r] | maybeFuncName fexp `elem` map Just comparisonOps -> Just (getExprLineCount expr, TBool)
  ACall fexp _
    | maybeFuncName fexp `elem` map Just (comparisonOps ++ ["print"]) -> Nothing
    | Just name <- maybeFuncName fexp -> getFunctionReturnType (typeAliases env) name
    | otherwise -> Nothing
  AMethodCall _ _ _ -> Nothing

comparisonOps :: [String]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

arithOps :: [String]
arithOps = ["+", "-", "*", "/"]

inferAccessType :: AstAccess -> CompilerEnv -> Maybe Type
inferAccessType acc env = case unwrapAccess acc of
  AArrayAccess arrExpr _ ->
    case inferType arrExpr env of
      Just t -> case unwrapType (stripWrap (resolveType env t)) of
        TArray et _ -> Just et
        TVector et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  AVectorAccess vecExpr _ ->
    case inferType vecExpr env of
      Just t -> case unwrapType (stripWrap (resolveType env t)) of
        TVector et _ -> Just et
        TArray et _ -> Just et
        _ -> Nothing
      Nothing -> Nothing
  ATupleAccess tupleExpr idxExpr ->
    case inferType tupleExpr env of
      Just t -> case unwrapType (stripWrap (resolveType env t)) of
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
        case unwrapType (stripWrap (resolveType env t)) of
          TStruct sname ->
            case M.lookup sname (structDefs env) of
              Just fds ->
                case lookup f (map (\(ty, nm) -> (nm, ty)) fds) of
                  Just fty -> go fty fs
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Nothing

tupleIndexType' :: [Type] -> AExpression -> Maybe Type
tupleIndexType' ts expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    ANumber (AInteger i)
      | i >= 0 && i < length ts -> Just (ts !! i)
      | otherwise -> Nothing
    _ -> Nothing
  _ -> Nothing

inferHomogeneousList :: LineCount -> (Type -> AExpression -> TypeRaw) -> [AExpression] -> CompilerEnv -> Maybe Type
inferHomogeneousList lc ctor exprs env =
  case traverse (\e -> inferType e env) exprs of
    Just [] -> Just (lc, ctor (lc, TInt) ((lc, AValue (lc, ANumber (AInteger 0)))))
    Just (t:ts) | all (typesEqual t) ts -> Just (lc, ctor t ((lc, AValue (lc, ANumber (AInteger (length exprs))))))
    _ -> Nothing

unwrapAst :: Ast -> AstRaw
unwrapAst (_, raw) = raw

unwrapExpr :: AExpression -> AExpressionRaw
unwrapExpr (_, raw) = raw

unwrapType :: Type -> TypeRaw
unwrapType (_, raw) = raw

unwrapAccess :: AstAccess -> AstAccessRaw
unwrapAccess (_, raw) = raw

unwrapValue :: AstValue -> AstValueRaw
unwrapValue (_, raw) = raw

getAstLineCount :: Ast -> LineCount
getAstLineCount (lc, _) = lc

getExprLineCount :: AExpression -> LineCount
getExprLineCount (lc, _) = lc

getTypeLineCount :: Type -> LineCount
getTypeLineCount (lc, _) = lc

getAccessLineCount :: AstAccess -> LineCount
getAccessLineCount (lc, _) = lc

getValueLineCount :: AstValue -> LineCount
getValueLineCount (lc, _) = lc

isLValue :: AExpression -> Bool
isLValue expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    AVarCall _ -> True
    _ -> False
  AAccess acc -> case unwrapAccess acc of
    AArrayAccess _ _ -> True
    AVectorAccess _ _ -> True
    ATupleAccess _ _ -> True
    AStructAccess _ _ -> True
  _ -> False

isTemporaryValue :: AExpression -> Bool
isTemporaryValue expr = case unwrapExpr expr of
  ACall _ _ -> True
  ACast _ _ -> True
  AValue val -> case unwrapValue val of
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
checkReferenceValidity expr lc
  | isTemporaryValue expr = 
      Left $ ReferenceToTemporary 
        ("Cannot create reference to temporary value: " ++ show expr) lc
  | not (isLValue expr) = 
      Left $ InvalidReference 
        ("Cannot create reference to non-lvalue expression: " ++ show expr) lc
  | otherwise = Right ()

checkDereferenceValidity :: AExpression -> Type -> LineCount -> Either CompilerError ()
checkDereferenceValidity _ exprType lc = 
  case unwrapType (stripWrap exprType) of
    TRef _ -> Right ()
    _ -> Left $ InvalidReference 
           ("Attempting to dereference non-reference type: " ++ show exprType) lc

validateReferences :: AExpression -> CompilerEnv -> M.Map String Type -> Either CompilerError ()
validateReferences expr env typeEnv = case unwrapExpr expr of
  ACast targetType innerExpr ->
    validateReferences innerExpr env typeEnv >>= \() ->
      case unwrapType (stripWrap (resolveType env targetType)) of
        TRef _ -> checkReferenceValidity innerExpr (getExprLineCount expr)
        _ -> Right ()
  ACall funcExpr args ->
    validateReferences funcExpr env typeEnv >>= \() ->
      mapM_ (\arg -> validateReferences arg env typeEnv) args >>= \() ->
        case maybeFuncName funcExpr of
          Just fname -> case getFunctionArgTypes typeEnv fname of
            Just argTypes -> validateRefArgs args argTypes (getExprLineCount expr)
            Nothing -> Right ()
          Nothing -> Right ()
  AAccess acc -> validateAccessReferences acc env typeEnv
  AValue val -> case unwrapValue val of
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
validateRefArgs (arg:args) (t:ts) lc =
  case unwrapType (stripWrap t) of
    TRef _ -> checkReferenceValidity arg lc >>= \() -> validateRefArgs args ts lc
    _ -> validateRefArgs args ts lc
validateRefArgs _ _ _ = Right ()

validateAccessReferences :: AstAccess -> CompilerEnv -> M.Map String Type -> Either CompilerError ()
validateAccessReferences acc env typeEnv = case unwrapAccess acc of
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
hasReturn ast = case unwrapAst ast of
  AReturn _ -> True
  ABlock asts -> any hasReturn asts
  AIf _ thenBranch elseBranch -> 
    case elseBranch of
      Just elseAst -> hasReturn thenBranch && hasReturn elseAst
      Nothing -> False
  ALoop _ _ _ _ -> False
  _ -> False

checkFunctionReturn :: Type -> [Ast] -> LineCount -> Either CompilerError ()
checkFunctionReturn retType body lc = 
  case unwrapType (stripWrap retType) of
    TInt -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TFloat -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TBool -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TChar -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TString -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TArray _ _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TVector _ _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TTuple _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TStruct _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    TRef _ -> case any hasReturn body of
      True -> Right ()
      False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lc
    _ -> Right ()

checkReturnType :: AExpression -> Type -> CompilerEnv -> LineCount -> Either CompilerError ()
checkReturnType expr expectedType env lc =
  case inferType expr env of
    Just actualType ->
      case eqTypeNormalized expectedType actualType of
        True -> Right ()
        False -> case (bothNumeric expectedType actualType) of
          True -> Right ()
          False -> Left $ InvalidReturnType 
            ("Return type mismatch: expected " ++ show expectedType ++ ", got " ++ show actualType) lc
    Nothing -> Right ()

validateReturnsInBody :: [Ast] -> Type -> CompilerEnv -> Either CompilerError ()
validateReturnsInBody asts expectedType env = 
  mapM_ (validateReturnsInAst expectedType env) asts

validateReturnsInAst :: Type -> CompilerEnv -> Ast -> Either CompilerError ()
validateReturnsInAst expectedType env ast = case unwrapAst ast of
  AReturn returnAst -> case unwrapAst returnAst of
    AExpress expr -> checkReturnType expr expectedType env (getAstLineCount ast)
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
checkMainSignature funcType lc = case unwrapType funcType of
  TKonst innerT -> case unwrapType innerT of
    TFunc [] retType -> case unwrapType (stripWrap retType) of
      TInt -> Right ()
      _ -> Left $ InvalidMainSignature "Main function must return Int" lc
    _ -> Left $ InvalidMainSignature "Main function must have no parameters and return Int" lc
  TFunc [] retType -> case unwrapType (stripWrap retType) of
    TInt -> Right ()
    _ -> Left $ InvalidMainSignature "Main function must return Int" lc
  _ -> Left $ InvalidMainSignature "Main function must have no parameters and return Int" lc

validateStructFieldAccess :: CompilerEnv -> String -> String -> LineCount -> Either CompilerError Type
validateStructFieldAccess env structName fieldName lc =
  case M.lookup structName (structDefs env) of
    Nothing -> Left $ UndefinedStruct ("Struct '" ++ structName ++ "' is not defined") lc
    Just fds ->
      case lookup fieldName (map (\(ty, nm) -> (nm, ty)) fds) of
        Nothing -> Left $ UnknownStructField 
          ("Field '" ++ fieldName ++ "' does not exist in struct '" ++ structName ++ "'") lc
        Just fieldType -> Right fieldType

validateTupleIndexAccess :: [Type] -> AExpression -> LineCount -> Either CompilerError Type
validateTupleIndexAccess ts idxExpr lc = case unwrapExpr idxExpr of
  AValue val -> case unwrapValue val of
    ANumber (AInteger i)
      | i < 0 -> Left $ IndexOutOfBounds 
          ("Tuple index " ++ show i ++ " is negative") lc
      | i >= length ts -> Left $ IndexOutOfBounds 
          ("Tuple index " ++ show i ++ " is out of bounds (tuple has " ++ show (length ts) ++ " elements)") lc
      | otherwise -> Right (ts !! i)
    _ -> Left $ InvalidArguments "Tuple index must be a constant integer" lc
  _ -> Left $ InvalidArguments "Tuple index must be a constant integer" lc

validateArrayIndexAccess :: Type -> AExpression -> AExpression -> LineCount -> Either CompilerError ()
validateArrayIndexAccess _ sizeExpr idxExpr lc = 
  case (unwrapExpr sizeExpr, unwrapExpr idxExpr) of
    (AValue sizeVal, AValue idxVal) -> 
      case (unwrapValue sizeVal, unwrapValue idxVal) of
        (ANumber (AInteger size), ANumber (AInteger idx))
          | idx < 0 -> Left $ IndexOutOfBounds 
              ("Array index " ++ show idx ++ " is negative") lc
          | idx >= size -> Left $ IndexOutOfBounds 
              ("Array index " ++ show idx ++ " is out of bounds (array size is " ++ show size ++ ")") lc
          | otherwise -> Right ()
        _ -> Right ()
    _ -> Right ()

validateAccess :: AstAccess -> CompilerEnv -> LineCount -> Either CompilerError ()
validateAccess acc env lc = case unwrapAccess acc of
  AArrayAccess arrExpr idxExpr -> case inferType arrExpr env of
    Just t -> case unwrapType (stripWrap (resolveType env t)) of
      TArray elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lc
      TVector elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lc
      _ -> Left $ InvalidArguments "Array access on non-array type" lc
    Nothing -> Right ()
  AVectorAccess vecExpr idxExpr -> case inferType vecExpr env of
    Just t -> case unwrapType (stripWrap (resolveType env t)) of
      TVector elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lc
      TArray elemType sizeExpr -> validateArrayIndexAccess elemType sizeExpr idxExpr lc
      _ -> Left $ InvalidArguments "Vector access on non-vector type" lc
    Nothing -> Right ()
  ATupleAccess tupExpr idxExpr -> case inferType tupExpr env of
    Just t -> case unwrapType (stripWrap (resolveType env t)) of
      TTuple ts -> validateTupleIndexAccess ts idxExpr lc >>= \_ -> Right ()
      _ -> Left $ InvalidArguments "Tuple access on non-tuple type" lc
    Nothing -> Right ()
  AStructAccess structExpr fds -> 
    validateStructAccess structExpr fds env lc

validateStructAccess :: AExpression -> [String] -> CompilerEnv -> LineCount -> Either CompilerError ()
validateStructAccess expr fds env lc = case inferType expr env of
  Just t0 -> go t0 fds
  Nothing -> Right ()
  where
    go _ [] = Right ()
    go t (f:fs) = case unwrapType (stripWrap (resolveType env t)) of
      TStruct sname ->
        validateStructFieldAccess env sname f lc >>= \fieldType ->
          go fieldType fs
      _ -> Left $ InvalidArguments "Struct access on non-struct type" lc

isPrimitiveType :: String -> Bool
isPrimitiveType typeName = typeName `elem` ["Int", "Float", "String", "Char", "Bool", "Void", "Array", "Vector"]

validateStructDefinition :: CompilerEnv -> String -> [(Type, String)] -> LineCount -> Either CompilerError ()
validateStructDefinition env structName fds linec = 
  mapM_ validateField fds
  where
    validateField (fieldType, _) = validateTypeExists structName env fieldType linec
    
    validateTypeExists :: String -> CompilerEnv -> Type -> LineCount -> Either CompilerError ()
    validateTypeExists sName envir ty linecount = case unwrapType ty of
      TCustom typeName
        | isPrimitiveType typeName -> Right ()
        | otherwise -> case M.lookup typeName (typeAliases envir) of
            Just _ -> Right ()
            Nothing -> case M.lookup typeName (structDefs envir) of
              Just _ -> Right ()
              Nothing -> Left $ UndefinedStruct 
                ("Type '" ++ typeName ++ "' used in struct '" ++ sName ++ "' is not defined") linecount
      TArray elemType _ -> validateTypeExists sName envir elemType linecount
      TVector elemType _ -> validateTypeExists sName envir elemType linecount
      TTuple types -> mapM_ (\t -> validateTypeExists sName envir t linecount) types
      TKonst innerType -> validateTypeExists sName envir innerType linecount
      TStrong innerType -> validateTypeExists sName envir innerType linecount
      TKong innerType -> validateTypeExists sName envir innerType linecount
      TRef innerType -> validateTypeExists sName envir innerType linecount
      TStruct sname -> 
        case M.lookup sname (structDefs env) of
          Just _ -> Right ()
          Nothing -> Left $ UndefinedStruct 
            ("Struct '" ++ sname ++ "' used in struct '" ++ sName ++ "' is not defined") linecount
      _ -> Right ()

validateDivisionByZero :: AExpression -> AExpression -> LineCount -> Either CompilerError ()
validateDivisionByZero _lhs rhs lc = case unwrapExpr rhs of
  AValue val -> case unwrapValue val of
    ANumber (AInteger 0) -> Left $ DivisionByZero "Division by literal zero" lc
    ANumber (AFloat 0.0) -> Left $ DivisionByZero "Division by literal zero" lc
    _ -> Right ()
  _ -> Right ()

validateConstantBounds :: Type -> AstNumber -> LineCount -> Either CompilerError ()
validateConstantBounds ty num lc = 
  case (unwrapType ty, num) of
    (TInt, AInteger n) 
      | toInteger n < -2147483648 || toInteger n > 2147483647 -> 
          Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for int") lc
    (TStrong innerT, AInteger n) -> case unwrapType innerT of
      TInt | toInteger n < -9223372036854775808 || toInteger n > 9223372036854775807 -> 
          Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for long") lc
      TKong innerInnerT -> case unwrapType innerInnerT of
        TInt | toInteger n < 0 || toInteger n > 18446744073709551615 -> 
            Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for unsigned long") lc
        _ -> Right ()
      _ -> Right ()
    (TKong innerT, AInteger n) -> case unwrapType innerT of
      TInt | toInteger n < 0 || toInteger n > 4294967295 -> 
          Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for unsigned int") lc
      TStrong innerInnerT -> case unwrapType innerInnerT of
        TInt | toInteger n < 0 || toInteger n > 18446744073709551615 -> 
            Left $ ConstantOverflow ("Integer constant " ++ show n ++ " out of bounds for unsigned long") lc
        _ -> Right ()
      _ -> Right ()
    _ -> Right ()

isValidCast :: Type -> Type -> CompilerEnv -> LineCount -> Either CompilerError ()
isValidCast sourceType targetType env lc =
  case (unwrapType (stripWrap (resolveType env sourceType)), unwrapType (stripWrap (resolveType env targetType))) of
    (s, t) | isNumericType' s && isNumericType' t -> Right ()
    (s, t) -> Left $ InvalidCast 
      ("Cannot cast from " ++ show s ++ " to " ++ show t) lc
  where
    isNumericType' t = case t of
      TInt -> True
      TBool -> True
      TChar -> True
      TFloat -> True
      _ -> False

validateArithmeticOperands :: String -> Type -> Type -> LineCount -> Either CompilerError ()
validateArithmeticOperands op t1 t2 lc =
  case (stripWrap t1, stripWrap t2) of
    (type1, type2) | numericCompatible type1 type2 -> Right ()
    (type1, type2) -> Left $ IncompatibleOperands 
      ("Arithmetic operator '" ++ op ++ "' cannot be applied to types " ++ show type1 ++ " and " ++ show type2) lc

validateNonCallable :: String -> Type -> LineCount -> Either CompilerError ()
validateNonCallable varName varType lc =
  case unwrapType varType of
    TKonst _ -> Right ()
    _ -> Left $ NonCallableType 
      ("'" ++ varName ++ "' of type " ++ show varType ++ " is not callable") lc

validateKonstAssignment :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateKonstAssignment var env lc = 
  case M.lookup var (typeAliases env) of
    Just t | isKonst t -> Left $ KonstModification 
      ("Cannot modify Konst variable '" ++ var ++ "'") lc
    _ -> Right ()
