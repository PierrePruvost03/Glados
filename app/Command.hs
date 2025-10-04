{-# LANGUAGE LambdaCase #-}
module Command (printHelp, printSymbols) where

import Interpreter.BaseEnv
import DataStruct.Value
import Data.Map

commandList :: [(String, String)]
commandList = [
        ("help", "display the command list"),
        ("load", "execute the content of a file and load his symbols"),
        ("type", "display the type of a symbol")]

printCommandHelp :: (String, String) -> IO ()
printCommandHelp (n, d) = putStrLn (':':(n <> "\t" <> d))

printHelp :: IO ()
printHelp = f commandList
    where
        f [] = return ()
        f (x:xs) = printCommandHelp x >> f xs

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing a = a

printType :: Env -> [String] -> IO ()
printType _ [] = return ()
printType env (x:xs) = orElse (lookupEnv x env >>=
        \v -> Just (putStrLn (show x <> ": " <> show v)))
    (putStrLn (show x <> ": unkow symbol")) >> printType env xs

printSymbols :: Env -> [String] -> IO ()
printSymbols env [] = f (assocs env)
    where
        f [] = return ()
        f ((n, v):xs) = putStrLn (show n <> ": " <> show v) >> f xs
printSymbols env l = printType env l
