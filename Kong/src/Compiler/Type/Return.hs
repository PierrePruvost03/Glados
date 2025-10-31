module Compiler.Type.Return
  ( hasReturn
  , checkFunctionReturn
  , checkReturnType
  , validateReturnsInBody
  , validateReturnsInAst
  , checkMainSignature
  , isNonVoidType
  ) where

import DataStruct.Ast
import Parser (LineCount)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import Compiler.Type.Normalization (stripWrap, eqTypeNormalized)
import Compiler.Type.Checks (bothNumeric)
import Compiler.Type.Inference (CompilerEnv, inferType)
import Compiler.Type.Error (CompilerError(..))

-- Check if an AST contains at least one return statement
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

-- Check that a function with non-void return type contains a return statement
checkFunctionReturn :: Type -> [Ast] -> LineCount -> Either CompilerError ()
checkFunctionReturn retType body lineCount
  | isNonVoidType (unwrap (stripWrap retType)) = 
      case any hasReturn body of
        True -> Right ()
        False -> Left $ MissingReturn "Function with non-void return type must have a return statement" lineCount
  | otherwise = Right ()

isNonVoidType :: TypeRaw -> Bool
isNonVoidType TInt = True
isNonVoidType TFloat = True
isNonVoidType TBool = True
isNonVoidType TChar = True
isNonVoidType TString = True
isNonVoidType (TArray _ _) = True
isNonVoidType (TVector _ _) = True
isNonVoidType (TTuple _) = True
isNonVoidType (TStruct _) = True
isNonVoidType (TRef _) = True
isNonVoidType _ = False

-- Check that the returned expression type matches the function's expected return type
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

-- Recursively validate all returns in a list of AST nodes
validateReturnsInBody :: [Ast] -> Type -> CompilerEnv -> Either CompilerError ()
validateReturnsInBody asts expectedType env = 
  mapM_ (validateReturnsInAst expectedType env) asts

-- Recursively validate all returns in an AST node (checks that each return has correct type)
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

-- Check that main function has correct signature: () -> Int
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
