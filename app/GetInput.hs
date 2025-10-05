module GetInput (getUserInput) where

import AstParsing (parseAstFromSExpr)
import DataStruct.SExpr (SExpr)
import DataStruct.Value (Env)
import GHC.IO.StdHandles (stdin)
import DataStruct.Command
import Interpreter.EvalAst (evalAst)
import Parser (Parser (runParser), ParsingError, Rest)
import SExprParser (parseLisp)
import GHC.IO.Handle
import CommandParser
import GHC.IO.Handle.FD (openFile)
import Control.Exception
import System.IO (IOMode(ReadMode))
import System.IO (hPutStrLn, stderr)
import Command

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

parseToCommand :: String -> Either ParsingError (Command, Rest)
parseToCommand str = runParser parseCommand (str, (0, 0))

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

handleInput :: String -> Env -> Handle -> IO Env
handleInput str env fd = case parseToSExpr str of
    Right (sexpr, _) -> handleSExpr sexpr env >>= \newEnv -> getUserInput "" newEnv fd
    Left ("nothing", _, _) -> getUserInput "" env fd
    Left err -> print err >> getUserInput "" env fd

checkCompare :: Either String String -> Env -> Handle -> IO Env
checkCompare (Right str) env fd = getUserInput str env fd
checkCompare (Left str) env fd = case parseToCommand str of
    Right (command, _)
        | fd == stdin -> execCommand env command
            >>= \newEnv -> getUserInput "" newEnv fd
        | otherwise -> handleInput str env fd
    Left _ -> handleInput str env fd

getUserInput :: String -> Env -> Handle -> IO Env
getUserInput txt env fd = hIsEOF fd >>= \x -> checkEof x
  where
    checkEof :: Bool -> IO Env
    checkEof True = pure env
    checkEof _ = hGetLine fd >>= \content ->
        checkCompare (useCompare $ txt <> content <> "\n") env fd



-- COMMAND GESTION

loadFiles :: Env -> [String] -> IO Env
loadFiles env [] = pure env
loadFiles env (x:xs) = catch (openFile x ReadMode >>=
    \file -> putStrLn ("file " <> show x <> " successfully oppened") >> getUserInput "" env file) handler >>=
    \newEnv -> loadFiles newEnv xs
    where
        handler :: IOException -> IO Env
        handler e = hPutStrLn stderr ("Could not open file \"" <> x <> "\": " <> show e)
            >> pure env

execCommand :: Env -> Command -> IO Env
execCommand env ("help", _) = printHelp >> pure env
execCommand env ("load", files) = loadFiles env files
execCommand env ("type", symbols) = printSymbols env symbols >> pure env
execCommand env (s, _) = print
    (s <> ": Command not found. use :help to get all the commands")
    >> pure env
