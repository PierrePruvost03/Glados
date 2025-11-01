module Compiler.BytecodeGen.Expr.Helpers
  ( maybeFuncName
  , elementTypeFromResolved
  , lookupResolved
  , pushVarValue
  , compileNumber
  , compileNumberWithType
  , typeToNumberType
  , isRefTypeWrapped
  , checkAccessType
  , checkLambdaReturn
  , isReturnStmt
  , extendWithDecl
  , extendScopeWithPrefixDecls
  , isAssignmentCall
  , isComparisonCall
  , isArithmeticCall
  , isLogicalCall
  , isPrintCall
  , extractVariableName
  , buildLambdaEnv
  , getCapturedNames
  , compileLambdaParams
  , isDivisionOp
  , isFunctionType
  , addPrintSyscall
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..), NumberType(..))
import DataStruct.Bytecode.Value (Instr(..))
import DataStruct.Bytecode.Syscall (Syscall(..))
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), resolveType)
import Compiler.Type.Checks (isKonst)
import Compiler.Type.Return (checkFunctionReturn, validateReturnsInBody)
import Compiler.Unwrap (Unwrappable(..))
import Compiler.BytecodeGen.Utils (extractParamNames, extractGlobalNames)
import qualified Data.Map as M
import qualified Data.List as L
import Parser (LineCount)

-- Extract function name from an expression if it's a simple variable call
maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall n -> Just n
    _ -> Nothing
  _ -> Nothing

-- Extract the element type from a resolved array/vector type
elementTypeFromResolved :: Type -> Maybe Type
elementTypeFromResolved t = case unwrap t of
  TArray et _ -> Just et
  TVector et _ -> Just et
  TKonst ty -> elementTypeFromResolved ty
  TStrong ty -> elementTypeFromResolved ty
  TKong ty -> elementTypeFromResolved ty
  _ -> Nothing

-- Lookup a variable and resolve its type
lookupResolved :: CompilerEnv -> String -> Maybe Type
lookupResolved env v =
  case M.lookup v (typeAliases env) of
    Just t  -> Just (resolveType env t)
    Nothing -> Nothing

-- Generate instructions to push a variable's value onto the stack
pushVarValue :: CompilerEnv -> String -> [Instr]
pushVarValue env vname
  | Just t <- M.lookup vname (typeAliases env)
  , not (isKonst (resolveType env t)) = [PushEnv vname, LoadRef]
  | otherwise = [PushEnv vname]

-- Convert AST number to bytecode number
compileNumber :: AstNumber -> Number
compileNumber (AInteger n) = VInt (fromIntegral n)
compileNumber (AFloat f) = VFloat (f)
compileNumber (ABool b) = VBool b
compileNumber (AChar c) = VChar c

compileNumberWithType :: AstNumber -> Maybe Type -> CompilerEnv -> Number
compileNumberWithType (AInteger n) (Just expectedType) env = 
  case unwrap (resolveType env expectedType) of
    TStrong ty -> case unwrap ty of
      TInt -> VLong (fromIntegral n)
      _ -> VInt (fromIntegral n)
    TKong ty -> case unwrap ty of
      TInt -> VUInt (fromIntegral n)
      _ -> VInt (fromIntegral n)
    TKonst ty -> compileNumberWithType (AInteger n) (Just ty) env
    _ -> VInt (fromIntegral n)
compileNumberWithType number Nothing _ = compileNumber number
compileNumberWithType number _ _ = compileNumber number

-- Convert a type to a NumberType for casting
typeToNumberType :: Type -> Maybe NumberType
typeToNumberType t = case unwrap t of
  TInt -> Just NTInt
  TBool -> Just NTBool
  TChar -> Just NTChar
  TFloat -> Just NTFloat
  TKonst ty -> typeToNumberType ty
  TStrong ty -> typeToNumberTypeForStrong (stripTypeModifiers ty)
  TKong ty -> typeToNumberTypeForKong (stripTypeModifiers ty)
  _ -> Nothing

typeToNumberTypeForStrong :: Type -> Maybe NumberType
typeToNumberTypeForStrong innerType = case unwrap innerType of
  TInt -> Just NTLong
  TFloat -> Just NTFloat
  _ -> Nothing

typeToNumberTypeForKong :: Type -> Maybe NumberType
typeToNumberTypeForKong innerType = case unwrap innerType of
  TInt -> Just NTUInt
  _ -> Nothing

stripTypeModifiers :: Type -> Type
stripTypeModifiers t = case unwrap t of
  TKonst ty -> stripTypeModifiers ty
  TStrong ty -> stripTypeModifiers ty
  TKong ty -> stripTypeModifiers ty
  _ -> t

-- Check if a type is a reference type
isRefTypeWrapped :: Type -> Bool
isRefTypeWrapped t = case unwrap t of
  TRef _ -> True
  TKonst ty -> isRefTypeWrapped ty
  TStrong ty -> isRefTypeWrapped ty
  TKong ty -> isRefTypeWrapped ty
  _ -> False

-- Validate that a type can be accessed (is array/vector/tuple/struct)
checkAccessType :: Maybe Type -> LineCount -> Either CompilerError ()
checkAccessType (Just t) lnCount = case unwrap t of
  TArray _ _ -> Right ()
  TVector _ _ -> Right ()
  TTuple _ -> Right ()
  TStruct _ -> Right ()
  TKonst ty -> checkAccessType (Just ty) lnCount
  TStrong ty -> checkAccessType (Just ty) lnCount
  TKong ty -> checkAccessType (Just ty) lnCount
  _ -> Left $ InvalidArguments "Invalid type for access (not a struct/array/vector)" lnCount
checkAccessType Nothing lnCount = Left $ InvalidArguments "Unable to infer type for access" lnCount

-- Check lambda return type and validate all return statements
checkLambdaReturn :: Type -> [Ast] -> CompilerEnv -> Either CompilerError ()
checkLambdaReturn expected bodyStmts scope =
  checkFunctionReturn expected bodyStmts ((0, 0)) >>= \() ->
    validateReturnsInBody bodyStmts expected (extendScopeWithPrefixDecls bodyStmts scope)

-- Check if an AST node is a return statement
isReturnStmt :: Ast -> Bool
isReturnStmt ast = case unwrap ast of
  AReturn _ -> True
  _ -> False

-- Extend environment with a variable declaration
extendWithDecl :: CompilerEnv -> Ast -> CompilerEnv
extendWithDecl env ast = case unwrap ast of
  AVarDecl t n _ -> env { typeAliases = M.insert n t (typeAliases env) }
  _ -> env

-- Extend scope with all declarations before the first return statement
extendScopeWithPrefixDecls :: [Ast] -> CompilerEnv -> CompilerEnv
extendScopeWithPrefixDecls stmts env = foldl extendWithDecl env (takeWhile (not . isReturnStmt) stmts)

isAssignmentCall :: AExpression -> Bool
isAssignmentCall fexp = maybeFuncName fexp == Just "="

isComparisonCall :: AExpression -> [String] -> Bool
isComparisonCall fexp ops = maybeFuncName fexp `elem` map Just ops

isArithmeticCall :: AExpression -> [String] -> Bool
isArithmeticCall fexp ops = maybeFuncName fexp `elem` map Just ops

isLogicalCall :: AExpression -> [String] -> Bool
isLogicalCall fexp ops = maybeFuncName fexp `elem` map Just ops

isPrintCall :: AExpression -> Bool
isPrintCall fexp = maybeFuncName fexp == Just "print"

-- Extract variable name from an expression if it's a simple variable
extractVariableName :: AExpression -> Maybe String
extractVariableName expr = case unwrap expr of
  AValue val -> case unwrap val of
    AVarCall name -> Just name
    _ -> Nothing
  _ -> Nothing


-- Build environment with lambda parameters added to type aliases
buildLambdaEnv :: [Ast] -> CompilerEnv -> CompilerEnv
buildLambdaEnv params env = foldl extendWithDecl env params

-- Get captured variable names (params + globals) for lambda closure
getCapturedNames :: [Ast] -> CompilerEnv -> [String]
getCapturedNames params env = L.nub (extractParamNames params ++ extractGlobalNames (typeAliases env))

-- Compile lambda parameter setup instructions
compileLambdaParams :: [Ast] -> [Instr]
compileLambdaParams params = concatMap compileSingleParam params

-- Helper to compile a single parameter setup
compileSingleParam :: Ast -> [Instr]
compileSingleParam p = case unwrap p of
  AVarDecl t pname _
    | isRefTypeWrapped t -> [SetVar pname]
    | otherwise -> [Alloc, StoreRef, SetVar pname]
  _ -> []

isDivisionOp :: String -> Bool
isDivisionOp op = op `elem` ["/", "%"]

-- Check if a type is a function type (TKonst)
isFunctionType :: Type -> Bool
isFunctionType t = case unwrap t of
  TKonst _ -> True
  _ -> False

-- Add a print syscall to compiled instructions
addPrintSyscall :: Int -> [Instr] -> [Instr]
addPrintSyscall n instrs = instrs ++ [Syscall (Print n)]
