{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Compiler.KongCompiler
  ( compile
  , compileIf
  , compileAst
  , compileProgram
  , CompilerError(..)
  , ProgramError(..)
  ) where

import DataStruct.Ast
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (Op(..), builtinOps, stringToOp)
import DataStruct.Bytecode.Value (Instr(..), Value(..), Env, MemoryCell)
import qualified Data.Map as M
import qualified Data.Vector as V

data CompilerError
  = UnsupportedAst String
  | UnknownVariable String
  | UnknownFunction String
  | InvalidArguments String
  deriving (Show, Eq)


data ProgramError = ProgramError
  { peFile :: String
  , peAst :: Ast
  , peError :: CompilerError
  } deriving (Show)

compileProgram :: [(String, [Ast])] -> Either [ProgramError] [Instr]
compileProgram = resultsToEither . map compilePair . expand

expand :: [(String, [Ast])] -> [(String, Ast)]
expand = concatMap (\(f, as) -> map ((,) f) as)

compilePair :: (String, Ast) -> Either ProgramError [Instr]
compilePair (f, a) = either (Left . ProgramError f a) Right (compile a)

resultsToEither :: [Either ProgramError [Instr]] -> Either [ProgramError] [Instr]
resultsToEither = foldr step (Right [])

step :: Either ProgramError [Instr] -> Either [ProgramError] [Instr] -> Either [ProgramError] [Instr]
step (Left e) (Left es) = Left (e : es)
step (Left e) (Right _) = Left [e]
step (Right _) (Left es) = Left es
step (Right is) (Right js) = Right (is ++ js)

getMemoryCell :: Env -> String -> MemoryCell
getMemoryCell env varName =
  case M.lookup varName env of
    Just (_, cell) -> cell
    Nothing -> error $ "Variable not found in environment: " ++ varName

compile :: Ast -> Either CompilerError [Instr]
compile ast = compileAst ast M.empty

compileAst :: Ast -> Env -> Either CompilerError [Instr]
compileAst ast env = case ast of
  ABlock asts ->
    fmap concat $ mapM (`compileAst` env) asts
  AFunkDef name params returnType body ->
    fmap (\instrs -> [Push (VFunction (extractParamNames params) (V.fromList (concat instrs ++ [Ret]))), SetVar name])
         (mapM (`compileAst` env) body)
  AVarDecl varType name Nothing ->
    Right [Push (defaultValue varType), SetVar name]
  AVarDecl _ name (Just value) ->
    fmap (++ [SetVar name]) (compileExpr value env)
  AExpress expr -> compileExpr expr env
  ASymbol symbol -> Right [PushEnv symbol]
  AReturn expr ->
    fmap (++ [Ret]) (compileAst expr env)
  AIf {} -> compileIf ast env
  ALoop {} -> compileLoop ast env
  _ -> Left $ UnsupportedAst (show ast)


-- if (x < y)
-- then
  -- zizi
  -- prout
-- 
-- 
-- pushEnv "x"
-- pushEnv "y"
-- DoOp Lt
-- JumpIfFalse  

compileIf :: Ast -> Env -> Either CompilerError [Instr]
compileIf (AIf (AExpress cond) thenBranch elseBranch) env =
  compileExpr cond env >>= \compiledCond ->
    compileAst thenBranch env >>= \compiledThen ->
      Right [JumpIfFalse (length compiledThen + 2)] >>= \condJump ->
        f elseBranch >>= \compiledElse ->
          Right [Jump (length compiledElse + 1)] >>= \thenJump ->
            Right (compiledCond <> condJump <> compiledThen <> thenJump <> compiledElse)
  where
    f (Just a) = compileAst a env
    f Nothing = Right []
compileIf _ _ = Left $ UnsupportedAst "If statement not supported"


compileLoop :: Ast -> Env -> Either CompilerError [Instr]
compileLoop (ALoop Nothing cond (Just incr) body) env =
  concat <$> sequence
  [compileAst cond env
    , Right [JumpIfFalse loopLength]
    , compiledBody
    , compiledIncr
    , Right [Jump (- loopLength)]
  ]
    where
      compiledBody = compileAst body env
      compiledIncr = compileAst incr env
      loopLength = length compiledBody + length compiledIncr + 2
compileLoop _ _ = Left $ UnsupportedAst "Loop not supported"

compileExpr :: AExpression -> Env -> Either CompilerError [Instr]
compileExpr expr env = case expr of
  AAttribution var value ->
    fmap (++ [SetVar var]) (compileExpr value env)
  ACall funcName args ->
    fmap (\argInstrs -> concat argInstrs ++ compileCall funcName)
         (mapM (`compileExpr` env) (reverse args))
  AValue astValue -> compileValue astValue env
  AAccess access -> compileAccess access env

compileCall :: String -> [Instr]
compileCall funcName
  | funcName `elem` builtinOps = [DoOp (stringToOp funcName)]
  | otherwise                  = [PushEnv funcName, Call]

compileValue :: AstValue -> Env -> Either CompilerError [Instr]
compileValue value env = case value of
  ANumber (AInteger n) -> Right [Push (VNumber (VInt n))]
  ANumber (AFloat f) -> Right [Push (VNumber (VFloat (realToFrac f)))]
  ANumber (ABool b) -> Right [Push (VNumber (VBool b))]
  ANumber (AChar c) -> Right [Push (VNumber (VChar c))]
  AString s -> Right [Push (VString s)]
  AVarCall varName -> Right [PushEnv varName]
  _ -> Left $ UnsupportedAst ("Unsupported value: " ++ show value)

compileAccess :: AstAccess -> Env -> Either CompilerError [Instr]
compileAccess access env = case access of
  AArrayAccess varName index ->
    fmap (([PushEnv varName] ++) . (++ [ArrayGet])) (compileExpr index env)
  _ -> Left $ UnsupportedAst ("Unsupported access: " ++ show access)

extractParamNames :: [Ast] -> [String]
extractParamNames = foldr extractParam []
  where
    extractParam (ASymbol name) acc = name : acc
    extractParam _ acc = acc

defaultValue :: Type -> Value
defaultValue = \case
  TInt -> VNumber $ VInt 0
  TBool -> VNumber $ VBool False
  TChar -> VNumber $ VChar '\0'
  TFloat -> VNumber $ VFloat 0.0
  TString -> VString ""
  TStrong t -> defaultValue t
  TKong t -> defaultValue t
  _ -> VEmpty
