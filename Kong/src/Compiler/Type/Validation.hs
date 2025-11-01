module Compiler.Type.Validation
  ( validateStructFieldAccess
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
  , checkFunctionCallTypes
  , checkAssignmentType
  ) where

import DataStruct.Ast
import Parser (LineCount)
import qualified Data.Map as M
import Compiler.Unwrap (Unwrappable(..))
import Compiler.Type.Normalization (stripWrap, eqTypeNormalized)
import Compiler.Type.Checks (numericCompatible, isKonst, isRefType, extractRefType)
import Compiler.Type.Inference (CompilerEnv(..), inferType, resolveType)
import Compiler.Type.Error (CompilerError(..))

-- Checks that struct exists and field is defined in it
validateStructFieldAccess :: CompilerEnv -> String -> String -> LineCount -> Either CompilerError Type
validateStructFieldAccess env structName fieldName lnCount =
  case M.lookup structName (structDefs env) of
    Nothing -> Left $ UndefinedStruct structName "" lnCount
    Just fds ->
      case lookup fieldName (map (\(ty, nm) -> (nm, ty)) fds) of
        Nothing -> Left $ UnknownStructField structName fieldName lnCount
        Just fieldType -> Right fieldType

-- Validate tuple element access by index (index must be constant integer within bounds)
validateTupleIndexAccess :: [Type] -> AExpression -> LineCount -> Either CompilerError Type
validateTupleIndexAccess ts idxExpr lineCount = case unwrap idxExpr of
  AValue val -> case unwrap val of
    ANumber (AInteger i)
      | i < 0 -> Left $ NegativeIndex i lineCount
      | i >= length ts -> Left $ IndexOutOfBounds i (length ts) lineCount
      | otherwise -> Right (ts !! i)
    _ -> Left $ InvalidArguments "Tuple index must be a constant integer" lineCount
  _ -> Left $ InvalidArguments "Tuple index must be a constant integer" lineCount

-- Validate array/vector element access by index
validateArrayIndexAccess :: Type -> AExpression -> AExpression -> LineCount -> Either CompilerError ()
validateArrayIndexAccess _ sizeExpr idxExpr lineCount = 
  case (unwrap sizeExpr, unwrap idxExpr) of
    (AValue sizeVal, AValue idxVal) -> 
      case (unwrap sizeVal, unwrap idxVal) of
        (ANumber (AInteger size), ANumber (AInteger idx))
          | idx < 0 -> Left $ NegativeIndex idx lineCount
          | idx >= size -> Left $ IndexOutOfBounds idx size lineCount
          | otherwise -> Right ()
        _ -> Right ()
    _ -> Right ()

-- Validate an access expression (array, vector, tuple, struct)
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

-- Validate chained struct access
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

-- Check if a type name is a primitive type
isPrimitiveType :: String -> Bool
isPrimitiveType typeName = typeName `elem` ["Int", "Float", "String", "Char", "Bool", "Void", "Array", "Vector"]

-- Checks that all field types exist and are defined
validateStructDefinition :: CompilerEnv -> String -> [(Type, String)] -> LineCount -> Either CompilerError ()
validateStructDefinition env structName fds lineCount = mapM_ validateField fds
  where
    validateField (fieldType, _) = validateTypeExists structName env fieldType lineCount

-- validate that a type is defined
validateTypeExists :: String -> CompilerEnv -> Type -> LineCount -> Either CompilerError ()
validateTypeExists sName env ty lnCount = case unwrap ty of
  TCustom typeName -> validateCustomType sName env typeName lnCount
  TArray elemType _ -> validateTypeExists sName env elemType lnCount
  TVector elemType _ -> validateTypeExists sName env elemType lnCount
  TTuple types -> mapM_ (\t -> validateTypeExists sName env t lnCount) types
  TKonst innerType -> validateTypeExists sName env innerType lnCount
  TStrong innerType -> validateTypeExists sName env innerType lnCount
  TKong innerType -> validateTypeExists sName env innerType lnCount
  TRef innerType -> validateTypeExists sName env innerType lnCount
  TStruct sname -> case M.lookup sname (structDefs env) of
    Just _ -> Right ()
    Nothing -> Left $ UndefinedStruct sname sName lnCount
  _ -> Right ()

-- accept primitives, aliases or defined structs
validateCustomType :: String -> CompilerEnv -> String -> LineCount -> Either CompilerError ()
validateCustomType sName env typeName lnCount
  | isPrimitiveType typeName = Right ()
  | otherwise = case M.lookup typeName (typeAliases env) of
      Just _ -> Right ()
      Nothing -> case M.lookup typeName (structDefs env) of
        Just _ -> Right ()
        Nothing -> Left $ UndefinedStruct typeName sName lnCount

-- Check that division is not by zero
validateDivisionByZero :: AExpression -> AExpression -> LineCount -> Either CompilerError ()
validateDivisionByZero _lhs rhs lineCount = case unwrap rhs of
  AValue val -> case unwrap val of
    ANumber (AInteger 0) -> Left $ DivisionByZero lineCount
    ANumber (AFloat 0.0) -> Left $ DivisionByZero lineCount
    _ -> Right ()
  _ -> Right ()

-- Check that integer constants will not overflow
validateConstantBounds :: Type -> AstNumber -> LineCount -> Either CompilerError ()
validateConstantBounds ty num lineCount = 
  case (unwrap ty, num) of
    (TInt, AInteger n) 
      | toInteger n < -2147483648 || toInteger n > 2147483647 -> 
          Left $ ConstantOverflow (toInteger n) "int" lineCount
    (TStrong innerT, AInteger n) -> case unwrap innerT of
      TInt | toInteger n < -9223372036854775808 || toInteger n > 9223372036854775807 -> 
          Left $ ConstantOverflow (toInteger n) "long" lineCount
      TKong innerInnerT -> case unwrap innerInnerT of
        TInt | toInteger n < 0 || toInteger n > 18446744073709551615 -> 
            Left $ ConstantOverflow (toInteger n) "unsigned long" lineCount
        _ -> Right ()
      _ -> Right ()
    (TKong innerT, AInteger n) -> case unwrap innerT of
      TInt | toInteger n < 0 || toInteger n > 4294967295 -> 
          Left $ ConstantOverflow (toInteger n) "unsigned int" lineCount
      TStrong innerInnerT -> case unwrap innerInnerT of
        TInt | toInteger n < 0 || toInteger n > 18446744073709551615 -> 
            Left $ ConstantOverflow (toInteger n) "unsigned long" lineCount
        _ -> Right ()
      _ -> Right ()
    _ -> Right ()

-- Check that a cast is valid (only numeric types can be cast to each other)
isValidCast :: Type -> Type -> CompilerEnv -> LineCount -> Either CompilerError ()
isValidCast sourceType targetType env lineCount =
  case (unwrap (stripWrap (resolveType env sourceType)), unwrap (stripWrap (resolveType env targetType))) of
    (s, t) | isNumericType' s && isNumericType' t -> Right ()
    _ -> Left $ InvalidCast sourceType targetType lineCount
  where
    isNumericType' t = case t of
      TInt -> True
      TBool -> True
      TChar -> True
      TFloat -> True
      _ -> False

-- Check that operands of an arithmetic operation are of compatible types
validateArithmeticOperands :: String -> Type -> Type -> LineCount -> Either CompilerError ()
validateArithmeticOperands op t1 t2 lineCount =
  case (stripWrap t1, stripWrap t2) of
    (type1, type2) | numericCompatible type1 type2 -> Right ()
    (type1, type2) -> Left $ IncompatibleOperands op type1 type2 lineCount

-- Check that a variable called as a function is actually callable
validateNonCallable :: String -> Type -> LineCount -> Either CompilerError ()
validateNonCallable var t lineCount =
  case unwrap t of
    TKonst _ -> Right ()
    _ -> Left $ NonCallableType var t lineCount

-- Check that user trying to modify a constant variable
validateKonstAssignment :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateKonstAssignment var env lineCount = 
  case M.lookup var (typeAliases env) of
    Just t | isKonst t -> Left $ KonstModification var lineCount
    _ -> Right ()

-- Check that a variable/function name is not already declared in current scope
validateNoDuplicateDeclaration :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateNoDuplicateDeclaration name env lineCount =
  case M.lookup name (typeAliases env) of
    Just _ -> Left $ DuplicateDeclaration name "Variable or function" lineCount
    Nothing -> Right ()

-- Check that a struct name is not already declared
validateNoDuplicateStruct :: String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateNoDuplicateStruct name env lineCount =
  case M.lookup name (structDefs env) of
    Just _ -> Left $ DuplicateDeclaration name "Struct" lineCount
    Nothing -> Right ()

-- Check that function call arguments match expected parameter types
checkFunctionCallTypes :: LineCount -> [Type] -> [Maybe Type] -> Either CompilerError ()
checkFunctionCallTypes lnCount (t:ts) (Just a:as)
  | eqTypeNormalized t a || numericCompatible t a = checkFunctionCallTypes lnCount ts as
  | isRefType t && eqTypeNormalized (extractRefType t) a = checkFunctionCallTypes lnCount ts as
  | otherwise = Left $ InvalidArguments ("Function argument type mismatch: expected " ++ show t ++ ", got " ++ show a) lnCount
checkFunctionCallTypes lnCount (_:_) (Nothing:_) = Left $ InvalidArguments "Unable to infer argument type" lnCount
checkFunctionCallTypes lnCount [] remaining@(_:_) = Left $ InvalidArguments ("Too many arguments: expected 0 more, got " ++ show (length remaining)) lnCount
checkFunctionCallTypes lnCount remaining@(_:_) [] = Left $ InvalidArguments ("Too few arguments: expected " ++ show (length remaining) ++ " more") lnCount
checkFunctionCallTypes _ [] [] = Right ()

-- Check that assignment types match (with reference unwrapping support and vector size flexibility)
checkAssignmentType :: LineCount -> Maybe Type -> Maybe Type -> Either CompilerError ()
checkAssignmentType lnCount (Just expected) (Just actual)
  | eqTypeNormalized expected actual = Right ()
  | isRefType expected && eqTypeNormalized (extractRefType expected) actual = Right ()
  | checkVectorSizeFlexibility expected actual = Right ()
  | otherwise = Left $ IllegalAssignment ("Type mismatch on assignment: expected " ++ show expected ++ ", got " ++ show actual) lnCount
checkAssignmentType lnCount _ _ = Left $ IllegalAssignment "Unable to infer types for assignment" lnCount

-- Check if vector assignment is flexible (size 0 accepts any size, and any size accepts size 0)
checkVectorSizeFlexibility :: Type -> Type -> Bool
checkVectorSizeFlexibility expected actual =
  case (unwrap (stripWrapForAssignment expected), unwrap (stripWrapForAssignment actual)) of
    (TVector expectedElemType expectedSizeExpr, TVector actualElemType actualSizeExpr) ->
      -- Elements are compatible (or one is TInt for empty vector)
      (eqTypeNormalized expectedElemType actualElemType || 
       (isZeroSize actualSizeExpr && isTInt actualElemType) ||
       (isZeroSize expectedSizeExpr && isTInt expectedElemType))
      &&
      -- Expected has size 0 (dynamic size) or actual has size 0 (empty literal)
      (isZeroSize expectedSizeExpr || isZeroSize actualSizeExpr)
    _ -> False
  where
    isZeroSize :: AExpression -> Bool
    isZeroSize sizeExpr = case unwrap sizeExpr of
      AValue val -> case unwrap val of
        ANumber (AInteger 0) -> True
        _ -> False
      _ -> False
    isTInt :: Type -> Bool
    isTInt t = case unwrap t of
      TInt -> True
      _ -> False

-- Strip type wrappers for assignment checking (similar to stripTypeWrappers)
stripWrapForAssignment :: Type -> Type
stripWrapForAssignment t = case unwrap t of
  TKonst x -> stripWrapForAssignment x
  TStrong x -> stripWrapForAssignment x
  TKong x -> stripWrapForAssignment x
  _ -> t
