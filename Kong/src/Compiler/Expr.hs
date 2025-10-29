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
import Compiler.Types (CompilerError(..), CompilerEnv(..), resolveType, unwrapExpr, unwrapValue, unwrapAccess, unwrapType, unwrapAst, getExprLineCount, getAstLineCount, getAccessLineCount, getValueLineCount)
import Compiler.TypeError (prettyTypeError)
import Compiler.Types (isKonst, checkComparisonTypes, inferType, checkAssignmentType, comparisonOps, arithOps, numericCompatible, getFunctionArgTypes, checkFunctionCallTypes, eqTypeNormalized, bothNumeric)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.List as L
import Compiler.Block (compileAstWith)
import Parser (LineCount)

maybeFuncName :: AExpression -> Maybe String
maybeFuncName expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    AVarCall n -> Just n
    _ -> Nothing
  _ -> Nothing

compileExpr :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileExpr expr env = case unwrapExpr expr of
  AAttribution var rhs ->
    case M.lookup var (typeAliases env) of
      Nothing -> Left (UnknownVariable var (getExprLineCount expr))
      Just _ -> matchAssignment (M.lookup var (typeAliases env)) (inferType rhs env) (compileExpr rhs env)
    where
      matchAssignment t v (Right rhsCode) =
        case checkAssignmentType (getExprLineCount expr) t v of
          Right () -> Right (rhsCode ++ [PushEnv var, StoreRef])
          Left err -> Left err
      matchAssignment _ _ (Left err) = Left err
  AValue astValue -> compileValue astValue env
  AAccess access -> compileAccess access env
  ACast targetType ex -> compileCast targetType ex env (getExprLineCount expr)
  AMethodCall _ _ _ -> Left $ UnsupportedAst "Method calls not yet implemented" (getExprLineCount expr)
  ACall fexp [lhs, val] | maybeFuncName fexp == Just "=" ->
    compileAssignmentExpr lhs val env (getExprLineCount expr)
  ACall fexp [lh, rh] | maybeFuncName fexp `elem` map Just comparisonOps ->
    compileComparisonExpr fexp lh rh env (getExprLineCount expr)
  ACall fexp [lh, rh] | maybeFuncName fexp `elem` map Just arithOps ->
    compileArithmeticExpr fexp lh rh env (getExprLineCount expr)
  ACall fexp args | maybeFuncName fexp == Just "print" -> compilePrintCall args env (getExprLineCount expr)
  ACall fexp args -> compileFunctionCall fexp args env (getExprLineCount expr)

compileAssignmentExpr :: AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileAssignmentExpr lhs val env lc = case unwrapExpr lhs of
  -- x = val
  AValue lval -> case unwrapValue lval of
    AVarCall v ->
      case checkAssignmentType lc (M.lookup v (typeAliases env)) (inferType val env) of
        Right () -> (++) <$> compileExpr val env <*> Right [PushEnv v, StoreRef]
        Left err -> Left err
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment" lc
  -- arr[idx] = val, vec<idx> = val, tup|idx| = val
  AAccess acc -> case unwrapAccess acc of
    AArrayAccess nameExpr idx -> compileIndexedAssignment nameExpr idx val env lc False
    AVectorAccess nameExpr idx -> compileIndexedAssignment nameExpr idx val env lc False
    ATupleAccess nameExpr idx -> compileTupleAssignment nameExpr idx val env lc
    AStructAccess nameExpr [field] -> compileStructAssignment nameExpr field val env lc
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment" lc
  _ -> Left $ InvalidArguments "Invalid left-hand side for assignment" lc

compileIndexedAssignment :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Bool -> Either CompilerError [Instr]
compileIndexedAssignment nameExpr idx val env lc _isTuple = case unwrapExpr nameExpr of
  AValue nval -> case unwrapValue nval of
    AVarCall name ->
      case checkAssignmentType lc (case lookupResolved env name of
                                  Just rt -> elementTypeFromResolved rt
                                  Nothing -> Nothing)
                               (inferType val env) of
        Right () -> (++) <$> ((++) <$> compileExpr val env <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                          <*> Right [SetList, PushEnv name, StoreRef]
        Left err -> Left err
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (base must be a variable)" lc
  _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (base must be a variable)" lc

compileTupleAssignment :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileTupleAssignment nameExpr idx val env lc = case unwrapExpr nameExpr of
  AValue nval -> case unwrapValue nval of
    AVarCall name ->
      case checkAssignmentType lc (case lookupResolved env name of
                                  Just t -> case unwrapType t of
                                    TTuple ts -> tupleIndexType ts idx
                                    _ -> Nothing
                                  _ -> Nothing)
                               (inferType val env) of
        Right () -> (++) <$> ((++) <$> compileExpr val env <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                          <*> Right [SetList, PushEnv name, StoreRef]
        Left err -> Left err
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (tuple base must be a variable)" lc
  _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (tuple base must be a variable)" lc

compileStructAssignment :: AExpression -> String -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileStructAssignment nameExpr field val env lc = case unwrapExpr nameExpr of
  AValue nval -> case unwrapValue nval of
    AVarCall name ->
      case checkAssignmentType lc (case lookupResolved env name of
                                  Just t -> case unwrapType t of
                                    TStruct sname ->
                                      case M.lookup sname (structDefs env) of
                                        Just fds -> lookup field (map (\(ty, nm) -> (nm, ty)) fds)
                                        Nothing  -> Nothing
                                    _ -> Nothing
                                  _ -> Nothing)
                               (inferType val env) of
        Right () -> (++) <$> ((++) <$> compileExpr val env <*> Right (pushVarValue env name))
                          <*> Right [SetStruct field, PushEnv name, StoreRef]
        Left err -> Left err
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (struct base must be a variable)" lc
  _ -> Left $ InvalidArguments "Invalid left-hand side for assignment (struct base must be a variable)" lc

compileComparisonExpr :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileComparisonExpr fexp lhs rhs env lc =
  case (inferType lhs env, inferType rhs env) of
    (Just t1, Just t2) ->
      case (compileExpr lhs env, compileExpr rhs env) of
        (Right lcode, Right rcode) ->
          case checkComparisonTypes t1 t2 of
            Right () -> case maybeFuncName fexp of
              Just opName -> Right (rcode ++ lcode ++ [DoOp (stringToOp opName)])
              Nothing -> Left $ InvalidArguments "Invalid comparison operator expression" lc
            Left terr -> Left $ InvalidArguments (prettyTypeError terr) lc
        (Left e, _) -> Left e
        (_, Left e) -> Left e
    _ -> Left $ InvalidArguments "Unable to infer types for comparison" lc

compileArithmeticExpr :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileArithmeticExpr fexp lhs rhs env lc =
  case (inferType lhs env, inferType rhs env) of
    (Just t1, Just t2)
      | numericCompatible t1 t2 -> case maybeFuncName fexp of
          Just opName -> (++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp opName)])
          Nothing -> Left $ InvalidArguments "Invalid arithmetic operator expression" lc
      | otherwise -> Left $ InvalidArguments ("Arithmetic operation on incompatible types: " ++ show t1 ++ ", " ++ show t2) lc
    _ -> Left $ InvalidArguments "Unable to infer types for arithmetic operation" lc

compileFunctionCall :: AExpression -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileFunctionCall fexp args env lc =
  case maybeFuncName fexp of
    Just name | name `elem` (comparisonOps ++ arithOps ++ ["open","read","write","close","exit"]) ->
      fmap (\compiledArgs -> concat compiledArgs ++ compileCall name) (mapM (`compileExpr` env) (reverse args))
    Just name ->
      matchFunctionCall name (M.lookup name (typeAliases env)) (getFunctionArgTypes (typeAliases env) name) (map (\a -> inferType a env) args) (compileArgsForCall env (getFunctionArgTypes (typeAliases env) name) args) lc
    Nothing ->
      (++) <$> (concat <$> mapM (`compileExpr` env) (reverse args))
           <*> ((++) <$> compileExpr fexp env <*> Right [Call])

matchFunctionCall :: String -> Maybe Type -> Maybe [Type] -> [Maybe Type] -> Either CompilerError [[Instr]] -> LineCount -> Either CompilerError [Instr]
matchFunctionCall funcName (Just t) (Just expectedTypes) argTypes (Right argInstrs) lc = case unwrapType t of
  TKonst _ ->
    case checkFunctionCallTypes lc expectedTypes argTypes of
      Right () -> Right (concat argInstrs ++ compileCall funcName)
      Left err -> Left err
  _ -> Left $ UnknownFunction funcName lc
matchFunctionCall _ (Just t) (Just _) _ (Left err) _lc = case unwrapType t of
  TKonst _ -> Left err
  _ -> Left err
matchFunctionCall funcName (Just t) Nothing _ _ lc = case unwrapType t of
  TKonst _ -> Left $ UnknownFunction funcName lc
  _ -> Left $ UnknownFunction funcName lc
matchFunctionCall funcName _ _ _ _ lc = Left $ UnknownFunction funcName lc

compileArgsForCall :: CompilerEnv -> Maybe [Type] -> [AExpression] -> Either CompilerError [[Instr]]
compileArgsForCall env (Just expectedTypes) args =
  traverse (uncurry $ compileArgForCall env) (reverse $ zip expectedTypes args)
compileArgsForCall env Nothing args =
  traverse (`compileExpr` env) (reverse args)

compileArgForCall :: CompilerEnv -> Type -> AExpression -> Either CompilerError [Instr]
compileArgForCall env expectedType arg
  | isRefTypeWrapped expectedType = compileAsReference arg env
  | otherwise = compileExpr arg env

isRefTypeWrapped :: Type -> Bool
isRefTypeWrapped t = case unwrapType t of
  TRef _ -> True
  TKonst ty -> isRefTypeWrapped ty
  TStrong ty -> isRefTypeWrapped ty
  TKong ty -> isRefTypeWrapped ty
  _ -> False

compileAsReference :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileAsReference expr env = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    AVarCall vname ->
      case M.lookup vname (typeAliases env) of
        Just _ -> Right [PushEnv vname]
        Nothing -> Left (UnknownVariable vname (getExprLineCount expr))
    _ -> compileExpr expr env
  _ -> compileExpr expr env

compilePrintCall :: [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compilePrintCall [arg] env _ = case unwrapExpr arg of
  AValue val -> case unwrapValue val of
    AString s -> Right [Push (VList (V.fromList (map (VNumber . VChar) s))), Syscall (Print 1)]
    ATuple xs -> fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileListLiteral xs env)
    AArray xs -> fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileListLiteral xs env)
    AVector xs -> fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileListLiteral xs env)
    _ -> fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileExpr arg env)
  _ -> fmap (\instrs -> instrs ++ [Syscall (Print 1)]) (compileExpr arg env)
compilePrintCall args env _ =
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
compileValue val env = case unwrapValue val of
  ANumber number -> Right [Push (VNumber (compileNumber number))]
  AString s -> Right [Push (VList (V.fromList (map (VNumber . VChar) s)))]
  ATuple exprs -> compileListLiteral exprs env
  AArray exprs -> compileListLiteral exprs env
  AVector exprs -> compileListLiteral exprs env
  AStruct structFields -> compileStructLiteral structFields env
  ALambda params retType body ->
    case checkLambdaReturn retType body env' of
      Left err -> Left err
      Right () ->
        fmap makeLambda (compileAstWith compileExpr (getValueLineCount val, ABlock body) env')
    where
      env' = foldl (\e p -> case unwrapAst p of { AVarDecl t n _ -> e { typeAliases = M.insert n t (typeAliases e) }; _ -> e }) env params
      paramNames = extractParamNames params
      globalNames = extractGlobalNames (typeAliases env)
      capturedNames = L.nub (paramNames ++ globalNames)
      genParam p = case unwrapAst p of
        AVarDecl t pname _
          | isRefTypeWrapped t -> [SetVar pname]
          | otherwise -> [Alloc, StoreRef, SetVar pname]
        _ -> []
      makeLambda (bodyCode, _) = [Push (VFunction capturedNames (V.fromList (concatMap genParam params ++ bodyCode)))]
  AVarCall vname ->
    case M.lookup vname (typeAliases env) of
      Just t
        | isRefTypeWrapped (resolveType env t) -> Right [PushEnv vname, LoadRef]  -- References: load the address they point to
        | not (isKonst (resolveType env t)) -> Right [PushEnv vname, LoadRef]  -- Non-const vars
        | otherwise -> Right [PushEnv vname]  -- Const vars
      Nothing -> Left (UnknownVariable vname (getValueLineCount val))

elementTypeFromResolved :: Type -> Maybe Type
elementTypeFromResolved t = case unwrapType t of
  TArray et _ -> Just et
  TVector et _ -> Just et
  TKonst ty -> elementTypeFromResolved ty
  TStrong ty -> elementTypeFromResolved ty
  TKong ty -> elementTypeFromResolved ty
  _ -> Nothing

lookupResolved :: CompilerEnv -> String -> Maybe Type
lookupResolved env v =
  case M.lookup v (typeAliases env) of
    Just t  -> Just (resolveType env t)
    Nothing -> Nothing

tupleIndexType :: [Type] -> AExpression -> Maybe Type
tupleIndexType ts expr = case unwrapExpr expr of
  AValue val -> case unwrapValue val of
    ANumber num -> case num of
      AInteger i
        | i >= 0 && i < length ts -> Just (ts !! i)
        | otherwise -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

checkLambdaReturn :: Type -> [Ast] -> CompilerEnv -> Either CompilerError ()
checkLambdaReturn expected bodyStmts scope =
  case getReturnExpr bodyStmts of
    Nothing -> Right ()
    Just ast -> case unwrapAst ast of
      AExpress e ->
        case inferType e (extendScopeWithPrefixDecls bodyStmts scope) of
             Just actual | eqTypeNormalized expected actual || bothNumeric expected actual -> Right ()
                         | otherwise -> Left $ InvalidArguments ("Return type mismatch: expected " ++ show expected ++ ", got " ++ show actual) (getAstLineCount ast)
             Nothing -> Right ()
      _ -> Right ()

isReturnStmt :: Ast -> Bool
isReturnStmt ast = case unwrapAst ast of
  AReturn _ -> True
  _ -> False

extendWithDecl :: CompilerEnv -> Ast -> CompilerEnv
extendWithDecl env ast = case unwrapAst ast of
  AVarDecl t n _ -> env { typeAliases = M.insert n t (typeAliases env) }
  _ -> env

extendScopeWithPrefixDecls :: [Ast] -> CompilerEnv -> CompilerEnv
extendScopeWithPrefixDecls stmts env = foldl extendWithDecl env (takeWhile (not . isReturnStmt) stmts)

getReturnExpr :: [Ast] -> Maybe Ast
getReturnExpr [] = Nothing
getReturnExpr (ast:xs) = case unwrapAst ast of
  AReturn _ -> Just ast
  _ -> getReturnExpr xs

checkAccessType :: Maybe Type -> LineCount -> Either CompilerError ()
checkAccessType (Just t) lc = case unwrapType t of
  TArray _ _ -> Right ()
  TVector _ _ -> Right ()
  TTuple _ -> Right ()
  TStruct _ -> Right ()
  TKonst ty -> checkAccessType (Just ty) lc
  TStrong ty -> checkAccessType (Just ty) lc
  TKong ty -> checkAccessType (Just ty) lc
  _ -> Left $ InvalidArguments "Invalid type for access (not a struct/array/vector)" lc
checkAccessType Nothing lc = Left $ InvalidArguments "Unable to infer type for access" lc

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess access env = case unwrapAccess access of
  AArrayAccess arrExpr idx ->
    matchAccess (getAccessLineCount access) (inferType arrExpr env)
      ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr arrExpr env) <*> Right [GetList])
  AVectorAccess vecExpr idx ->
    matchAccess (getAccessLineCount access) (inferType vecExpr env)
      ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr vecExpr env) <*> Right [GetList])
  ATupleAccess tupleExpr idx ->
    matchAccess (getAccessLineCount access) (inferType tupleExpr env)
      ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr tupleExpr env) <*> Right [GetList])
  AStructAccess structExpr fieldPath ->
    matchAccess (getAccessLineCount access) (inferType structExpr env)
      ((++) <$> compileExpr structExpr env <*> Right (map GetStruct fieldPath))

matchAccess :: LineCount -> Maybe Type -> Either CompilerError [Instr] -> Either CompilerError [Instr]
matchAccess lc t (Right code) =
  case checkAccessType t lc of
    Right () -> Right code
    Left err -> Left err
matchAccess _ _ (Left err) = Left err

pushVarValue :: CompilerEnv -> String -> [Instr]
pushVarValue env vname
  | Just t <- M.lookup vname (typeAliases env)
  , not (isKonst (resolveType env t)) = [PushEnv vname, LoadRef]
  | otherwise = [PushEnv vname]

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr step []
  where
    step ast acc = case unwrapAst ast of
      AVarDecl _ name _ -> name : acc
      _ -> acc

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

compileCast :: Type -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileCast targetType expr env lc =
  case typeToNumberType (resolveType env targetType) of
    Just numType ->
      case inferType expr env of
        Just exprType
          | isNumericType (resolveType env exprType) ->
              fmap (\exprCode -> exprCode ++ [Cast numType]) (compileExpr expr env)
          | otherwise ->
              Left $ InvalidArguments ("Cannot cast non-numeric type " ++ show exprType ++ " to " ++ show targetType) lc
        Nothing ->
          Left $ InvalidArguments "Unable to infer type of expression being cast" lc
    Nothing ->
      Left $ InvalidArguments ("Cannot cast to non-numeric type: " ++ show targetType) lc

typeToNumberType :: Type -> Maybe NumberType
typeToNumberType t = case unwrapType t of
  TInt -> Just NTInt
  TBool -> Just NTBool
  TChar -> Just NTChar
  TFloat -> Just NTFloat
  TKonst ty -> typeToNumberType ty
  TStrong ty -> typeToNumberType ty
  TKong ty -> typeToNumberType ty
  _ -> Nothing

isNumericType :: Type -> Bool
isNumericType t = case unwrapType t of
  TInt -> True
  TBool -> True
  TChar -> True
  TFloat -> True
  TKonst ty -> isNumericType ty
  TStrong ty -> isNumericType ty
  TKong ty -> isNumericType ty
  _ -> False
