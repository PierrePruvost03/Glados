module Main (main) where

-- import Lib

import AstParsing (parseAstFromSExpr)
import DataStruct.SExpr (SExpr)
import DataStruct.Value (Env)
import Interpreter.BaseEnv (defaultEnv)
import Interpreter.EvalAst (evalAst)
import Parser (Parser (runParser), ParsingError, Rest)
import SExprParser (parseLisp)
import System.Exit (exitSuccess)
import System.IO (isEOF)

checkParEnd :: String -> Int -> Bool
checkParEnd [] i = i > 0
checkParEnd ('(' : xs) i = checkParEnd xs (i + 1)
checkParEnd (')' : _) 0 = False
checkParEnd (')' : xs) i = checkParEnd xs (i - 1)
checkParEnd (_ : xs) i = checkParEnd xs i

isSkip :: Char -> Bool
isSkip ' ' = True
isSkip '\n' = True
isSkip '\t' = True
isSkip _ = False

parseToSExpr :: String -> Either ParsingError (SExpr, Rest)
parseToSExpr str = runParser parseLisp (str, (0, 0))

useCompare :: String -> Either String String
useCompare str
  | all isSkip str || checkParEnd str 0 = Right str
  | otherwise = Left str

handleSExpr :: SExpr -> Env -> IO Env
handleSExpr sexpr env = case parseAstFromSExpr sexpr of
  Right ast ->
    case evalAst env ast of
      Right (val, updatedEnv) -> print val >> pure updatedEnv
      Left err -> putStrLn err >> pure env
  Left err -> print err >> pure env


checkCompare :: Either String String -> Env -> IO ()
checkCompare (Right str) env = getUserInput str env
checkCompare (Left str) env = case parseToSExpr str of
  Right (sexpr, _) -> handleSExpr sexpr env >>= \newEnv -> getUserInput "" newEnv
  Left ("nothing", _, _) -> getUserInput "" env
  Left err -> print err >> getUserInput "" env

getUserInput :: String -> Env -> IO ()
getUserInput txt env = isEOF >>= \x -> checkEof x
  where
    checkEof :: Bool -> IO ()
    checkEof True = putStrLn "exit" >> exitSuccess
    checkEof _ = getLine >>= \content -> checkCompare (useCompare $ txt <> content) env

main :: IO ()
main = getUserInput "" defaultEnv
