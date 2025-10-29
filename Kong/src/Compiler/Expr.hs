module Compiler.Expr
  ( compileExpr
  , compileCall
  , compileValue
  , compileAccess
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..), NumberType(..))
import DataStruct.Bytecode.Op (builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Syscall (Syscall(..))
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType)
import Compiler.TypeError (prettyTypeError)
import Compiler.Types (isKonst, checkComparisonTypes, inferType, checkAssignmentType, comparisonOps, arithOps, numericCompatible, getFunctionArgTypes, checkFunctionCallTypes, eqTypeNormalized, bothNumeric)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.List as L
import Compiler.Block (compileAstWith)

maybeFuncName :: AExpression -> Maybe String
maybeFuncName (AValue (AVarCall n)) = Just n
maybeFuncName _ = Nothing

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
compileExpr (ACast targetType expr) env = compileCast targetType expr env
compileExpr (AMethodCall _ _ _) _ = Left $ UnsupportedAst "Method calls not yet implemented"
compileExpr (ACall fexp [lhs, val]) env | maybeFuncName fexp == Just "=" =
  case lhs of
    -- x = val
    AValue (AVarCall v) ->
      case checkAssignmentType (M.lookup v (typeAliases env)) (inferType val env) of
        Right () -> (++) <$> compileExpr val env <*> Right [PushEnv v, StoreRef]
        Left err -> Left err
    -- arr[idx] = val
    AAccess (AArrayAccess nameExpr idx) ->
      case nameExpr of
        AValue (AVarCall name) ->
          case checkAssignmentType (case lookupResolved env name of
                                      Just rt -> elementTypeFromResolved rt
                                      Nothing -> Nothing)
                                   (inferType val env) of
            Right () -> (++) <$> ((++) <$> compileExpr val env <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                              <*> Right [SetList, PushEnv name, StoreRef]
            Left err -> Left err
        _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (array base must be a variable)"
    -- vec<idx> = val
    AAccess (AVectorAccess nameExpr idx) ->
      case nameExpr of
        AValue (AVarCall name) ->
          case checkAssignmentType (case lookupResolved env name of
                                      Just rt -> elementTypeFromResolved rt
                                      Nothing -> Nothing)
                                   (inferType val env) of
            Right () -> (++) <$> ((++) <$> compileExpr val env <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                              <*> Right [SetList, PushEnv name, StoreRef]
            Left err -> Left err
        _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (vector base must be a variable)"
    -- tup|idx| = val
    AAccess (ATupleAccess nameExpr idx) ->
      case nameExpr of
        AValue (AVarCall name) ->
          case checkAssignmentType (case lookupResolved env name of
                                      Just (TTuple ts) -> tupleIndexType ts idx
                                      _ -> Nothing)
                                   (inferType val env) of
            Right () -> (++) <$> ((++) <$> compileExpr val env <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                              <*> Right [SetList, PushEnv name, StoreRef]
            Left err -> Left err
        _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (tuple base must be a variable)"
    -- struct{field} = val
    AAccess (AStructAccess nameExpr [field]) ->
      case nameExpr of
        AValue (AVarCall name) ->
          case checkAssignmentType (case lookupResolved env name of
                                      Just (TStruct sname) ->
                                        case M.lookup sname (structDefs env) of
                                          Just fds -> lookup field (map (\(ty, nm) -> (nm, ty)) fds)
                                          Nothing  -> Nothing
                                      _ -> Nothing)
                                   (inferType val env) of
            Right () -> (++) <$> ((++) <$> compileExpr val env <*> Right (pushVarValue env name))
                              <*> Right [SetStruct field, PushEnv name, StoreRef]
            Left err -> Left err
        _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (struct base must be a variable)"
    -- unsupported LHS
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment"
compileExpr (ACall fexp args) env | maybeFuncName fexp == Just "print" = compilePrintCall args env
compileExpr (ACall fexp [lhs, rhs]) env | maybeFuncName fexp `elem` map Just comparisonOps =
  case (inferType lhs env, inferType rhs env) of
    (Just t1, Just t2) ->
      case (compileExpr lhs env, compileExpr rhs env) of
        (Right lcode, Right rcode) ->
          case checkComparisonTypes t1 t2 of
            Right () -> case maybeFuncName fexp of
              Just opName -> Right (rcode ++ lcode ++ [DoOp (stringToOp opName)])
              Nothing -> Left $ InvalidArguments "Invalid comparison operator expression"
            Left terr -> Left $ InvalidArguments (prettyTypeError terr)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
    _ -> Left $ InvalidArguments "Unable to infer types for comparison"
compileExpr (ACall fexp [lhs, rhs]) env | maybeFuncName fexp `elem` map Just arithOps =
  case (inferType lhs env, inferType rhs env) of
    (Just t1, Just t2)
      | numericCompatible t1 t2 -> case maybeFuncName fexp of
          Just opName -> (++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp opName)])
          Nothing -> Left $ InvalidArguments "Invalid arithmetic operator expression"
      | otherwise -> Left $ InvalidArguments ("Arithmetic operation on incompatible types: " ++ show t1 ++ ", " ++ show t2)
    _ -> Left $ InvalidArguments "Unable to infer types for arithmetic operation"
compileExpr (ACall fexp args) env =
  case maybeFuncName fexp of
    Just name | name `elem` (comparisonOps ++ arithOps ++ ["print","open","read","write","close","exit"]) ->
      fmap (\compiledArgs -> concat compiledArgs ++ compileCall name) (mapM (`compileExpr` env) (reverse args))
    Just name ->
      matchFunctionCall name (M.lookup name (typeAliases env)) (getFunctionArgTypes (typeAliases env) name) (map (\a -> inferType a env) args) (compileArgsForCall env (getFunctionArgTypes (typeAliases env) name) args)
    Nothing ->
      (++) <$> (concat <$> mapM (`compileExpr` env) (reverse args))
           <*> ((++) <$> compileExpr fexp env <*> Right [Call])

matchFunctionCall :: String -> Maybe Type -> Maybe [Type] -> [Maybe Type] -> Either CompilerError [[Instr]] -> Either CompilerError [Instr]
matchFunctionCall funcName (Just (TKonst _)) (Just expectedTypes) argTypes (Right argInstrs) =
  case checkFunctionCallTypes expectedTypes argTypes of
    Right () -> Right (concat argInstrs ++ compileCall funcName)
    Left err -> Left err
matchFunctionCall _ (Just (TKonst _)) (Just _) _ (Left err) = Left err
matchFunctionCall funcName (Just (TKonst _)) Nothing _ _ = Left $ UnknownFunction funcName
matchFunctionCall funcName _ _ _ _ = Left $ UnknownFunction funcName

compileArgsForCall :: CompilerEnv -> Maybe [Type] -> [AExpression] -> Either CompilerError [[Instr]]
compileArgsForCall env (Just expectedTypes) args =
  traverse (uncurry $ compileArgForCall env) (reverse $ zip expectedTypes args)
compileArgsForCall env Nothing args =
  traverse (`compileExpr` env) (reverse args)

compileArgForCall :: CompilerEnv -> Type -> AExpression -> Either CompilerError [Instr]
compileArgForCall env expectedType arg
  | isRefType expectedType = compileAsReference arg env
  | otherwise = compileExpr arg env

isRefType :: Type -> Bool
isRefType (TRef _) = True
isRefType (TKonst t) = isRefType t
isRefType (TStrong t) = isRefType t
isRefType (TKong t) = isRefType t
isRefType _ = False

compileAsReference :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileAsReference (AValue (AVarCall vname)) env =
  case M.lookup vname (typeAliases env) of
    Just _ -> Right [PushEnv vname]
    Nothing -> Left (UnknownVariable vname)
compileAsReference expr env = compileExpr expr env

compilePrintCall :: [AExpression] -> CompilerEnv -> Either CompilerError [Instr]
compilePrintCall [AValue (AString s)] _ =
  Right [Push (VList (V.fromList (map (VNumber . VChar) s))), Syscall (Print 1)]
compilePrintCall [AValue (ATuple xs)] env =
  fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileListLiteral xs env)
compilePrintCall [AValue (AArray xs)] env =
  fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileListLiteral xs env)
compilePrintCall [AValue (AVector xs)] env =
  fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileListLiteral xs env)
compilePrintCall [arg] env =
  fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileExpr arg env)
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
compileValue (ALambda params retType body) env =
  case checkLambdaReturn retType body env' of
    Left err -> Left err
    Right () ->
      fmap makeLambda (compileAstWith compileExpr (ABlock body) env')
  where
    env' = foldl (\e p -> case p of { AVarDecl t n _ -> e { typeAliases = M.insert n t (typeAliases e) }; _ -> e }) env params
    paramNames = extractParamNames params
    globalNames = extractGlobalNames (typeAliases env)
    capturedNames = L.nub (paramNames ++ globalNames)
    genParam (AVarDecl t pname _)
      | isRefType t = [SetVar pname]
      | otherwise = [Alloc, StoreRef, SetVar pname]
    genParam _ = []
    makeLambda (bodyCode, _) = [Push (VFunction capturedNames (V.fromList (concatMap genParam params ++ bodyCode)))]
compileValue (AVarCall vname) env =
  case M.lookup vname (typeAliases env) of
    Just t
      | isRefType (resolveType env t) -> Right [PushEnv vname, LoadRef]  -- References: load the address they point to
      | not (isKonst (resolveType env t)) -> Right [PushEnv vname, LoadRef]  -- Non-const vars
      | otherwise -> Right [PushEnv vname]  -- Const vars
    Nothing -> Left (UnknownVariable vname)

elementTypeFromResolved :: Type -> Maybe Type
elementTypeFromResolved (TArray et _) = Just et
elementTypeFromResolved (TVector et _) = Just et
elementTypeFromResolved (TKonst t) = elementTypeFromResolved t
elementTypeFromResolved (TStrong t) = elementTypeFromResolved t
elementTypeFromResolved (TKong t) = elementTypeFromResolved t
elementTypeFromResolved _ = Nothing

lookupResolved :: CompilerEnv -> String -> Maybe Type
lookupResolved env v =
  case M.lookup v (typeAliases env) of
    Just t  -> Just (resolveType env t)
    Nothing -> Nothing

tupleIndexType :: [Type] -> AExpression -> Maybe Type
tupleIndexType ts (AValue (ANumber (AInteger i)))
  | i >= 0 && i < length ts = Just (ts !! i)
  | otherwise = Nothing
tupleIndexType _ _ = Nothing

checkLambdaReturn :: Type -> [Ast] -> CompilerEnv -> Either CompilerError ()
checkLambdaReturn expected bodyStmts scope =
  case getReturnExpr bodyStmts of
    Nothing -> Right ()
    Just (AExpress e) ->
      case inferType e (extendScopeWithPrefixDecls bodyStmts scope) of
           Just actual | eqTypeNormalized expected actual || bothNumeric expected actual -> Right ()
                       | otherwise -> Left $ InvalidArguments ("Return type mismatch: expected " ++ show expected ++ ", got " ++ show actual)
           Nothing -> Right ()
    _ -> Right ()

isReturnStmt :: Ast -> Bool
isReturnStmt (AReturn _) = True
isReturnStmt _ = False

extendWithDecl :: CompilerEnv -> Ast -> CompilerEnv
extendWithDecl env (AVarDecl t n _) = env { typeAliases = M.insert n t (typeAliases env) }
extendWithDecl env _ = env

extendScopeWithPrefixDecls :: [Ast] -> CompilerEnv -> CompilerEnv
extendScopeWithPrefixDecls stmts env = foldl extendWithDecl env (takeWhile (not . isReturnStmt) stmts)

getReturnExpr :: [Ast] -> Maybe Ast
getReturnExpr [] = Nothing
getReturnExpr (AReturn a : _ ) = Just a
getReturnExpr (_:xs) = getReturnExpr xs

checkAccessType :: Maybe Type -> Either CompilerError ()
checkAccessType (Just (TArray _ _)) = Right ()
checkAccessType (Just (TVector _ _)) = Right ()
checkAccessType (Just (TTuple _)) = Right ()
checkAccessType (Just (TStruct _)) = Right ()
checkAccessType (Just (TKonst t)) = checkAccessType (Just t)
checkAccessType (Just (TStrong t)) = checkAccessType (Just t)
checkAccessType (Just (TKong t)) = checkAccessType (Just t)
checkAccessType (Just _) = Left $ InvalidArguments "Invalid type for access (not a struct/array/vector)"
checkAccessType Nothing = Left $ InvalidArguments "Unable to infer type for access"

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess (AArrayAccess arrExpr idx) env =
  matchAccess (inferType arrExpr env)
    ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr arrExpr env) <*> Right [GetList])
compileAccess (AVectorAccess vecExpr idx) env =
  matchAccess (inferType vecExpr env)
    ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr vecExpr env) <*> Right [GetList])
compileAccess (ATupleAccess tupleExpr idx) env =
  matchAccess (inferType tupleExpr env)
    ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr tupleExpr env) <*> Right [GetList])
compileAccess (AStructAccess structExpr fieldPath) env =
  matchAccess (inferType structExpr env)
    ((++) <$> compileExpr structExpr env <*> Right (map GetStruct fieldPath))

matchAccess :: Maybe Type -> Either CompilerError [Instr] -> Either CompilerError [Instr]
matchAccess t (Right code) =
  case checkAccessType t of
    Right () -> Right code
    Left err -> Left err
matchAccess _ (Left err) = Left err

pushVarValue :: CompilerEnv -> String -> [Instr]
pushVarValue env vname
  | Just t <- M.lookup vname (typeAliases env)
  , not (isKonst (resolveType env t)) = [PushEnv vname, LoadRef]
  | otherwise = [PushEnv vname]

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr step []
  where
    step (AVarDecl _ name _) acc = name : acc
    step _ acc = acc

extractGlobalNames :: M.Map String a -> [String]
extractGlobalNames = M.keys

compileNumber :: AstNumber -> Number
compileNumber (AInteger n) = VInt n
compileNumber (AFloat f) = VFloat (realToFrac f)
compileNumber (ABool b) = VBool b
compileNumber (AChar c) = VChar c

compileListLiteral :: [AExpression] -> CompilerEnv -> Either CompilerError [Instr]
compileListLiteral exprs env =
  fmap (\compiled -> concat compiled ++ [CreateList (length exprs)])
       (mapM (`compileExpr` env) (reverse exprs))

compileStructLiteral :: [(String, AExpression)] -> CompilerEnv -> Either CompilerError [Instr]
compileStructLiteral fieldPairs env =
  fmap (\compiled -> concat compiled ++ [CreateStruct fieldNames])
       (mapM compileField (reverse fieldPairs))
  where
    compileField (_, expression) = compileExpr expression env
    fieldNames = map fst fieldPairs

compileCast :: Type -> AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileCast targetType expr env =
  case typeToNumberType (resolveType env targetType) of
    Just numType ->
      case inferType expr env of
        Just exprType
          | isNumericType (resolveType env exprType) ->
              fmap (\exprCode -> exprCode ++ [Cast numType]) (compileExpr expr env)
          | otherwise ->
              Left $ InvalidArguments ("Cannot cast non-numeric type " ++ show exprType ++ " to " ++ show targetType)
        Nothing ->
          Left $ InvalidArguments "Unable to infer type of expression being cast"
    Nothing ->
      Left $ InvalidArguments ("Cannot cast to non-numeric type: " ++ show targetType)

typeToNumberType :: Type -> Maybe NumberType
typeToNumberType TInt = Just NTInt
typeToNumberType TBool = Just NTBool
typeToNumberType TChar = Just NTChar
typeToNumberType TFloat = Just NTFloat
typeToNumberType (TKonst t) = typeToNumberType t
typeToNumberType (TStrong t) = typeToNumberType t
typeToNumberType (TKong t) = typeToNumberType t
typeToNumberType _ = Nothing

isNumericType :: Type -> Bool
isNumericType TInt = True
isNumericType TBool = True
isNumericType TChar = True
isNumericType TFloat = True
isNumericType (TKonst t) = isNumericType t
isNumericType (TStrong t) = isNumericType t
isNumericType (TKong t) = isNumericType t
isNumericType _ = False
