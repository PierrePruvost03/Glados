module Compiler.BytecodeGen.Expr.Expr
  ( compileExpr
  , compileExprWithType
  , compileCall
  , compileValue
  , compileAccess
  , compileIf
  , compileLoop
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Syscall (Syscall(..))
import Compiler.Type.Normalization (typeToString, stripWrap)
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (CompilerEnv(..), inferType, resolveType, getFunctionArgTypes, getTupleIndexType)
import Compiler.Type.Checks (isKonst, comparisonOps, arithOps, logicalOps, checkComparisonTypes)
import Compiler.Type.Validation (checkAssignmentType, checkFunctionCallTypes, validateAccess, validateDivisionByZero, isValidCast, validateKonstAssignment, validateArithmeticOperands, validateNonCallable)
import Compiler.Unwrap (Unwrappable(..), HasLineCount(..))
import qualified Data.Map as M
import qualified Data.Vector as V
import Compiler.BytecodeGen.Expr.Helpers (compileNumberWithType, addPrintSyscall, checkLambdaReturn, buildLambdaEnv, getCapturedNames, compileLambdaParams, lookupResolved, elementTypeFromResolved, extractVariableName, pushVarValue, checkAccessType, typeToNumberType, isRefTypeWrapped, isDivisionOp, isAssignmentCall, isComparisonCall, isLogicalCall, isArithmeticCall, isPrintCall, isFunctionType, maybeFuncName)
import Compiler.BytecodeGen.Block.Helpers (declareWithValue, validateInitializerValue)
import Compiler.BytecodeGen.Expr.Helpers
import Compiler.BytecodeGen.Block.Helpers (declareWithValue, canInitializeVectorWithDefault)
import Parser (LineCount)

-- MAIN EXPRESSION COMPILER - DISPATCHER

-- compile expression without expected type hint
compileExpr :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileExpr expr env = compileExprWithType expr env Nothing

-- Internal: compile expression with optional expected type for optimization
compileExprWithType :: AExpression -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr]
compileExprWithType expr env expectedType = case unwrap expr of
  AAttribution var rhs ->
    case M.lookup var (typeAliases env) of
      Nothing -> Left (UnknownVariable var (lc expr))
      Just vType ->
        validateKonstAssignment var env (lc expr) >>
        checkAssignmentType (lc expr) (Just vType) (inferType rhs env) >>
        fmap (\code -> code ++ [PushEnv var, StoreRef]) (compileExprWithType rhs env (Just vType))
  AValue astValue -> compileValueWithType astValue env expectedType
  AAccess access -> compileAccess access env
  ACast targetType ex -> compileCast targetType ex env (lc expr)
  AMethodCall obj methodName args -> compileMethodCall obj methodName args env (lc expr)
  ACall fexp [lhs, val] | isAssignmentCall fexp ->
    compileAssignmentExpr lhs val env (lc expr)
  ACall fexp [lh, rh] | isComparisonCall fexp comparisonOps ->
    compileComparisonExpr fexp lh rh env (lc expr)
  ACall fexp [lh, rh] | isLogicalCall fexp logicalOps ->
    compileLogicalExpr fexp lh rh env (lc expr)
  ACall fexp [lh, rh] | isArithmeticCall fexp arithOps ->
    compileArithmeticExpr fexp lh rh env (lc expr)
  ACall fexp args | isPrintCall fexp ->
    compilePrintCall args env (lc expr)
  ACall fexp args ->
    compileFunctionCall fexp args env (lc expr)


-- ASSIGNMENT EXPRESSIONS

compileAssignmentExpr :: AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileAssignmentExpr lhs val env lnCount = case unwrap lhs of
  -- x = val
  AValue lval -> case unwrap lval of
    AVarCall v ->
      validateKonstAssignment v env lnCount >>
      checkAssignmentType lnCount (M.lookup v (typeAliases env)) (inferType val env) >>
      fmap (++ [PushEnv v, StoreRef]) (compileExprWithType val env (M.lookup v (typeAliases env)))
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment" lnCount
  -- arr[idx] = val, vec<idx> = val, tup|idx| = val
  AAccess acc -> case unwrap acc of
    AArrayAccess nameExpr idx -> compileIndexedAssignment nameExpr idx val env lnCount False
    AVectorAccess nameExpr idx -> compileIndexedAssignment nameExpr idx val env lnCount False
    ATupleAccess nameExpr idx -> compileTupleAssignment nameExpr idx val env lnCount
    AStructAccess nameExpr [field] -> compileStructAssignment nameExpr field val env lnCount
    _ -> Left $ InvalidArguments "Invalid left-hand side for assignment" lnCount
  _ -> Left $ InvalidArguments "Invalid left-hand side for assignment" lnCount

compileIndexedAssignment :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Bool -> Either CompilerError [Instr]
compileIndexedAssignment nameExpr idx val env lnCount _isTuple =
  case extractVariableName nameExpr of
    Nothing -> Left $ InvalidArguments "Invalid left-hand side for assignment (base must be a variable)" lnCount
    Just name ->
      validateKonstAssignment name env lnCount >>
      case checkAssignmentType lnCount (getIndexedElementType env name) (inferType val env) of
        Right () -> (++) <$> ((++) <$> compileExprWithType val env (getIndexedElementType env name) <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                          <*> Right [SetList, PushEnv name, StoreRef]
        Left err -> Left err
  where
    getIndexedElementType e n = case lookupResolved e n of
      Just rt -> elementTypeFromResolved rt
      Nothing -> Nothing

compileTupleAssignment :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileTupleAssignment nameExpr idx val env lnCount =
  case extractVariableName nameExpr of
    Nothing -> Left $ InvalidArguments "Invalid left-hand side for assignment (tuple base must be a variable)" lnCount
    Just name ->
      validateKonstAssignment name env lnCount >>
      case checkAssignmentType lnCount (getTupleElementType env name idx) (inferType val env) of
        Right () -> (++) <$> ((++) <$> compileExprWithType val env (getTupleElementType env name idx) <*> ((++) <$> compileExpr idx env <*> Right (pushVarValue env name)))
                          <*> Right [SetList, PushEnv name, StoreRef]
        Left err -> Left err
  where
    getTupleElementType e n i = case lookupResolved e n of
      Just t -> case unwrap t of
        TTuple ts -> getTupleIndexType ts i
        _ -> Nothing
      _ -> Nothing

compileStructAssignment :: AExpression -> String -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileStructAssignment nameExpr field val env lnCount =
  case extractVariableName nameExpr of
    Nothing -> Left $ InvalidArguments "Invalid left-hand side for assignment (struct base must be a variable)" lnCount
    Just name ->
      validateKonstAssignment name env lnCount >>
      case checkAssignmentType lnCount (getStructFieldType env name field) (inferType val env) of
        Right () -> (++) <$> ((++) <$> compileExprWithType val env (getStructFieldType env name field) <*> Right (pushVarValue env name))
                          <*> Right [SetStruct field, PushEnv name, StoreRef]
        Left err -> Left err
  where
    getStructFieldType e n f = case lookupResolved e n of
      Just t -> case unwrap t of
        TStruct sname -> case M.lookup sname (structDefs e) of
          Just fds -> lookup f (map (\(ty, nm) -> (nm, ty)) fds)
          Nothing  -> Nothing
        _ -> Nothing
      _ -> Nothing


-- OPERATORS (COMPARISON & ARITHMETIC)

compileComparisonExpr :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileComparisonExpr fexp lhs rhs env lnCount =
  case (inferType lhs env, inferType rhs env, maybeFuncName fexp) of
    (Just t1, Just t2, Just opName) ->
      checkComparisonTypes t1 t2 lnCount >>
      ((++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp opName)]))
    (Just _, Just _, Nothing) -> Left $ InvalidLeftHandSide lnCount
    _ -> Left $ InvalidArguments "Unable to infer types for comparison" lnCount

compileArithmeticExpr :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileArithmeticExpr fexp lhs rhs env lnCount =
  case (inferType lhs env, inferType rhs env, maybeFuncName fexp) of
    (Just t1, Just t2, Just opName) | isDivisionOp opName ->
      validateArithmeticOperands opName t1 t2 lnCount >>
      validateDivisionByZero lhs rhs lnCount >>
      (++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp opName)])
    (Just t1, Just t2, Just opName) ->
      validateArithmeticOperands opName t1 t2 lnCount >>
      (++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp opName)])
    (Just _, Just _, Nothing) -> Left $ InvalidArguments "Invalid arithmetic operator expression" lnCount
    _ -> Left $ InvalidArguments "Unable to infer types for arithmetic operation" lnCount

compileLogicalExpr :: AExpression -> AExpression -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileLogicalExpr fexp lhs rhs env lnCount =
  case (inferType lhs env, inferType rhs env, maybeFuncName fexp) of
    (Just _, Just _, Just opName) ->
      (++) <$> compileExpr rhs env <*> ((++) <$> compileExpr lhs env <*> Right [DoOp (stringToOp opName)])
    (Just _, Just _, Nothing) -> Left $ InvalidArguments "Invalid logical operator expression" lnCount
    _ -> Left $ InvalidArguments "Unable to infer types for logical operation" lnCount


-- FUNCTION CALLS

compileFunctionCall :: AExpression -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileFunctionCall fexp args env lnCount =
  case maybeFuncName fexp of
    Just name | name `elem` (comparisonOps ++ arithOps ++ logicalOps), length args /= 2 ->
      Left $ ArgumentCountMismatch 2 (length args) lnCount
    Just name | name `elem` (comparisonOps ++ arithOps ++ logicalOps ++ ["open","read","write","close","exit"]) ->
      fmap (\compiledArgs -> concat compiledArgs ++ compileCall name) (mapM (`compileExpr` env) (reverse args))
    Just name ->
      case M.lookup name (typeAliases env) of
        Just vartype ->
          validateNonCallable name vartype lnCount >>
          validateFunctionAndCompileCall name (Just vartype) (getFunctionArgTypes (typeAliases env) name) (map (\a -> inferType a env) args) (compileArgsForCall env (getFunctionArgTypes (typeAliases env) name) args) lnCount
        Nothing -> validateFunctionAndCompileCall name Nothing Nothing (map (\a -> inferType a env) args) (compileArgsForCall env Nothing args) lnCount
    Nothing ->
      (++) <$> (concat <$> mapM (`compileExpr` env) (reverse args))
           <*> ((++) <$> compileExpr fexp env <*> Right [Call])

-- Validate function type, check argument types, and compile function call
validateFunctionAndCompileCall :: String -> Maybe Type -> Maybe [Type] -> [Maybe Type] -> Either CompilerError [[Instr]] -> LineCount -> Either CompilerError [Instr]
validateFunctionAndCompileCall funcName (Just t) (Just expectedTypes) argTypes (Right argInstrs) lnCount
  | isFunctionType t =
      checkFunctionCallTypes lnCount expectedTypes argTypes >>
      Right (concat argInstrs ++ compileCall funcName)
  | otherwise = Left $ UnknownFunction funcName lnCount
validateFunctionAndCompileCall _ (Just t) (Just _) _ (Left err) _lnCount
  | isFunctionType t = Left err
  | otherwise = Left err
validateFunctionAndCompileCall funcName (Just t) Nothing _ _ lnCount
  | isFunctionType t = Left $ UnknownFunction funcName lnCount
  | otherwise = Left $ UnknownFunction funcName lnCount
validateFunctionAndCompileCall funcName _ _ _ _ lnCount = Left $ UnknownFunction funcName lnCount

-- Compile arguments for a function call, handling expected types
compileArgsForCall :: CompilerEnv -> Maybe [Type] -> [AExpression] -> Either CompilerError [[Instr]]
compileArgsForCall env (Just expectedTypes) args =
  traverse (uncurry $ compileArgForCall env) (reverse $ zip expectedTypes args)
compileArgsForCall env Nothing args =
  traverse (`compileExpr` env) (reverse args)

compileArgForCall :: CompilerEnv -> Type -> AExpression -> Either CompilerError [Instr]
compileArgForCall env expectedType arg
  | isRefTypeWrapped expectedType = compileAsReference arg env
  | otherwise = compileExprWithType arg env (Just expectedType)

-- Compile an expression as a reference (for ref parameters)
compileAsReference :: AExpression -> CompilerEnv -> Either CompilerError [Instr]
compileAsReference expr env =
  case extractVariableName expr of
    Just vname ->
      case M.lookup vname (typeAliases env) of
        Just _ -> Right [PushEnv vname]
        Nothing -> Left (UnknownVariable vname (lc expr))
    Nothing -> compileExpr expr env

-- Compile the special print function call
compilePrintCall :: [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compilePrintCall [arg] env _ = case unwrap arg of
  AValue val -> case unwrap val of
    AString s -> Right [Push (VList (V.fromList (map (VNumber . VChar) s))), Syscall (Print 1)]
    ATuple xs -> fmap (addPrintSyscall 1) (compileListLiteral xs env)
    AArray xs -> fmap (addPrintSyscall 1) (compileListLiteral xs env)
    AVector xs -> fmap (addPrintSyscall 1) (compileListLiteral xs env)
    _ -> fmap (addPrintSyscall 1) (compileExpr arg env)
  _ -> fmap (addPrintSyscall 1) (compileExpr arg env)
compilePrintCall args env _ =
  fmap (addPrintSyscall (length args)) (concat <$> mapM (`compileExpr` env) (reverse args))

-- Generate instructions for calling a function (built-in or user-defined)
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


-- VALUES (LITERALS, VARIABLES, LAMBDAS)

compileValue :: AstValue -> CompilerEnv -> Either CompilerError [Instr]
compileValue val env = compileValueWithType val env Nothing

compileValueWithType :: AstValue -> CompilerEnv -> Maybe Type -> Either CompilerError [Instr]
compileValueWithType val env expectedType = case unwrap val of
  ANumber number -> Right [Push (VNumber (compileNumberWithType number expectedType env))]
  AString s -> Right [Push (VList (V.fromList (map (VNumber . VChar) s)))]
  ATuple exprs -> compileListLiteral exprs env
  AArray exprs -> compileListLiteral exprs env
  AVector exprs -> compileListLiteral exprs env
  AStruct structFields -> compileStructLiteral structFields env
  ALambda params retType body ->
    checkLambdaReturn retType body lambdaEnv >>= \() ->
      fmap makeLambdaValue (compileBlockForLambda body lambdaEnv)
    where
      lambdaEnv = buildLambdaEnv params env
      capturedNames = getCapturedNames params env
      paramInstrs = compileLambdaParams params
      makeLambdaValue (bodyCode, _) = [Push (VFunction capturedNames (V.fromList (paramInstrs ++ bodyCode)))]

      -- Compile a block of statements for a lambda body
      -- Simplified version that only handles what's needed inside lambdas
      compileBlockForLambda :: [Ast] -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
      compileBlockForLambda asts env' =
        foldl
          (\acc a -> acc >>= \(code, sc) ->
              compileSingleAst a sc >>= \(code', sc') -> Right (code ++ code', sc'))
          (Right ([], env'))
          asts

      -- Compile a single AST node inside a lambda
      compileSingleAst :: Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv)
      compileSingleAst ast env' = case unwrap ast of
        ABlock innerAsts -> compileBlockForLambda innerAsts env'
        AVarDecl t name (Just initExpr) ->
          validateInitializerValue t initExpr (lc ast) >>
          (case inferType initExpr env' of
            Just inferredType -> checkAssignmentType (lc ast) (Just t) (Just inferredType)
            Nothing -> Right ()) >>
          compileExprWithType initExpr env' (Just t) >>= \initCode ->
            Right (declareWithValue env' t name initCode)
        AVarDecl t name Nothing ->
          case canInitializeVectorWithDefault t of
            Just initInstrs -> Right (declareWithValue env' t name initInstrs)
            Nothing -> Left (UninitializedVariable ("Variable '" ++ name ++ "' must be initialized in lambda") (lc ast))
        AExpress expr -> fmap (\instrs -> (instrs, env')) (compileExpr expr env')
        AReturn expr -> compileSingleAst expr env' >>= \(instrs, _) -> Right (instrs ++ [Ret], env')
        AIf _ _ _ -> fmap (\instrs -> (instrs, env')) (compileIf compileExpr compileSingleAst ast env')
        ALoop _ _ _ _ -> fmap (\instrs -> (instrs, env')) (compileLoop compileExpr compileSingleAst ast env')
        other -> Left $ UnsupportedAst ("Unsupported in lambda: " ++ show other) (lc ast)
  AVarCall vname ->
    case M.lookup vname (typeAliases env) of
      Just t
        | isRefTypeWrapped (resolveType env t) -> Right [PushEnv vname, LoadRef]  -- References: load the address they point to
        | not (isKonst (resolveType env t)) -> Right [PushEnv vname, LoadRef]  -- Non-const vars
        | otherwise -> Right [PushEnv vname]  -- Const vars
      Nothing -> Left (UnknownVariable vname (lc val))


-- ACCESS EXPRESSIONS (ARRAY, VECTOR, TUPLE, STRUCT)

compileAccess :: AstAccess -> CompilerEnv -> Either CompilerError [Instr]
compileAccess access env =
  validateAccess access env (lc access) >>= \() ->
    case unwrap access of
      AArrayAccess arrExpr idx ->
        validateAccessAndCompile (lc access) (inferType arrExpr env)
          ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr arrExpr env) <*> Right [GetList])
      AVectorAccess vecExpr idx ->
        validateAccessAndCompile (lc access) (inferType vecExpr env)
          ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr vecExpr env) <*> Right [GetList])
      ATupleAccess tupleExpr idx ->
        validateAccessAndCompile (lc access) (inferType tupleExpr env)
          ((++) <$> ((++) <$> compileExpr idx env <*> compileExpr tupleExpr env) <*> Right [GetList])
      AStructAccess structExpr fieldPath ->
        validateAccessAndCompile (lc access) (inferType structExpr env)
          ((++) <$> compileExpr structExpr env <*> Right (map GetStruct fieldPath))

-- Validate access type and return compiled code
validateAccessAndCompile :: LineCount -> Maybe Type -> Either CompilerError [Instr] -> Either CompilerError [Instr]
validateAccessAndCompile lnCount t (Right code) =
  case checkAccessType t lnCount of
    Right () -> Right code
    Left err -> Left err
validateAccessAndCompile _ _ (Left err) = Left err


-- LITERALS (LISTS, STRUCTS, CASTING)

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

-- Compile a cast expression
compileCast :: Type -> AExpression -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileCast targetType expr env lnCount =
  case inferType expr env of
    Just exprType ->
      isValidCast exprType targetType env lnCount >>
      case typeToNumberType (resolveType env targetType) of
        Just numType ->
          fmap (\exprCode -> exprCode ++ [Cast numType]) (compileExpr expr env)
        Nothing ->
          Left $ InvalidCast exprType targetType lnCount
    Nothing ->
      Left $ InvalidArguments "Unable to infer type of expression being cast" lnCount


-- CONTROL FLOW (IF, LOOP)

-- Compile an if-else statement into bytecode with conditional jumps
compileIf :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
          -> (Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv))
          -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileIf exprCompiler astCompiler ast env = case unwrap ast of
  AIf condAst thenBranch elseBranch ->
    case unwrap condAst of
      AExpress cond ->
        exprCompiler cond env >>= \compiledCond ->
          astCompiler thenBranch env >>= \(compiledThen, _) ->
            case elseBranch of
              Just elseAst ->
                astCompiler elseAst env >>= \(compiledElse, _) ->
                  Right (compiledCond ++ [JumpIfFalse (length compiledThen + 2)] ++ compiledThen ++ [Jump (length compiledElse + 1)] ++ compiledElse)
              Nothing ->
                Right (compiledCond ++ [JumpIfFalse (length compiledThen + 1)] ++ compiledThen)
      _ -> Left $ UnsupportedAst "If condition must be an expression" (lc ast)
  _ -> Left $ UnsupportedAst "If statement not supported" (lc ast)

-- Compile a loop (for/while) into bytecode instructions
compileLoop :: (AExpression -> CompilerEnv -> Either CompilerError [Instr])
            -> (Ast -> CompilerEnv -> Either CompilerError ([Instr], CompilerEnv))
            -> Ast -> CompilerEnv -> Either CompilerError [Instr]
compileLoop _exprCompiler astCompiler ast env = case unwrap ast of
  ALoop initAst cond incr body ->
    f initAst env >>= \(compiledInit, newenv) ->
      astCompiler cond newenv >>= \(compiledCond, newenv') ->
        f incr newenv' >>= \(compiledIncr, newenv'') ->
          astCompiler body newenv'' >>= \(compiledBody, _) ->
            Right (compiledInit ++ compiledCond ++ [JumpIfFalse (length compiledBody + length compiledIncr + 2)] ++ compiledBody ++ compiledIncr ++ [Jump (- (length compiledBody + length compiledIncr + length compiledCond + 2))])
    where
      f (Just a) newEnv = astCompiler a newEnv
      f Nothing newEnv = Right ([], newEnv)
  _ -> Left $ UnsupportedAst "Loop not supported" (lc ast)

-- Compile a method call (obj.method(args))
compileMethodCall :: AExpression -> String -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileMethodCall obj methodName args env lnCount =
  maybe (Left $ InvalidArguments ("Unable to infer type of object for method call '" ++ methodName ++ "'") lnCount)
        (compileMethodWithType obj methodName args env lnCount)
        (inferType obj env)

-- Compile method call when we know the object type
compileMethodWithType :: AExpression -> String -> [AExpression] -> CompilerEnv -> LineCount -> Type -> Either CompilerError [Instr]
compileMethodWithType obj methodName args env lnCount objType =
  dispatchMethodCompilation (stripWrap objType) (getFunctionNameForMethod (stripWrap objType) methodName)
  where
    dispatchMethodCompilation :: Type -> String -> Either CompilerError [Instr]
    dispatchMethodCompilation t name = case unwrap t of
      TVector _ _ -> compileVectorMethod obj name args env lnCount
      TArray _ _ -> compileArrayMethod obj name args env lnCount
      _ -> compileTraitMethod obj name args env lnCount

-- Get function name for a method call
getFunctionNameForMethod :: Type -> String -> String
getFunctionNameForMethod t method = case unwrap t of
  TVector _ _ -> vectorMethodName method
  TArray _ _ -> arrayMethodName method
  _ -> typeToString t ++ "$" ++ method
  where
    vectorMethodName m = case m of
      "push" -> '$':m
      "pop" -> '$':m
      "len" -> '$':m
      _ -> typeToString t ++ "$" ++ m
    arrayMethodName "len" = "len"
    arrayMethodName m = typeToString t ++ "$" ++ m

-- Compile vector method (push, pop, len)
compileVectorMethod :: AExpression -> String -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileVectorMethod obj name args env lnCount = case name of
  "$push" -> compileBuiltinMethodCall obj name args env lnCount
  "$pop" -> compileBuiltinMethodCall obj name args env lnCount
  "$len" -> compileBuiltinMethodCall obj name args env lnCount
  _ -> compileTraitMethod obj name args env lnCount

-- Compile array method (len)
compileArrayMethod :: AExpression -> String -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileArrayMethod obj "len" args env lnCount = compileBuiltinMethodCall obj "len" args env lnCount
compileArrayMethod obj name args env lnCount = compileTraitMethod obj name args env lnCount

-- Compile regular trait method
compileTraitMethod :: AExpression -> String -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileTraitMethod obj name args env lnCount =
  compileFunctionCall (lc obj, AValue (lc obj, AVarCall name)) (obj : args) env lnCount

-- Compile builtin method call (push, pop, len)
compileBuiltinMethodCall :: AExpression -> String -> [AExpression] -> CompilerEnv -> LineCount -> Either CompilerError [Instr]
compileBuiltinMethodCall objExpr builtinName argExprs env lnCount =
  maybe (Left $ InvalidArguments ("Builtin method '" ++ builtinName ++ "' can only be called on variables, not expressions") lnCount)
        (compileBuiltinForVariable builtinName argExprs env lnCount)
        (extractVariableName objExpr)

-- Compile builtin method for a specific variable
compileBuiltinForVariable :: String -> [AExpression] -> CompilerEnv -> LineCount -> String -> Either CompilerError [Instr]
compileBuiltinForVariable builtinName argExprs env lnCount vName =
  validateConstForBuiltin vName builtinName env lnCount
    >>= const (compileBuiltinArgs argExprs vName builtinName env)

-- Validate that variable is not const for mutating methods
validateConstForBuiltin :: String -> String -> CompilerEnv -> LineCount -> Either CompilerError ()
validateConstForBuiltin vName builtinName env lnCount =
  maybe (Right ())
        (checkConstForMethod builtinName lnCount)
        (M.lookup vName (typeAliases env))
  where
    checkConstForMethod :: String -> LineCount -> Type -> Either CompilerError ()
    checkConstForMethod name lCount t = case (isKonst t, isMutatingMethod name) of
      (True, True) -> Left $ InvalidArguments ("Cannot call method '" ++ name ++ "' on const variable '" ++ vName ++ "'") lCount
      _ -> Right ()

    isMutatingMethod :: String -> Bool
    isMutatingMethod "$push" = True
    isMutatingMethod "$pop" = True
    isMutatingMethod _ = False

-- Compile arguments and generate instructions for builtin method
compileBuiltinArgs :: [AExpression] -> String -> String -> CompilerEnv -> Either CompilerError [Instr]
compileBuiltinArgs argExprs vName builtinName env =
  fmap (buildBuiltinInstrs vName builtinName) (traverse (`compileExpr` env) argExprs)
  where
    buildBuiltinInstrs :: String -> String -> [[Instr]] -> [Instr]
    buildBuiltinInstrs vn bn argsCode = concat argsCode ++ [PushEnv vn, PushEnv bn, Call]
