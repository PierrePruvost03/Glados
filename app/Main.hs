module Main (main) where

-- import Lib
import System.IO (isEOF)
import System.Exit (exitSuccess)

checkParEnd :: String -> Int -> Bool
checkParEnd [] i = i > 0
checkParEnd ('(':xs) i = checkParEnd xs (i + 1)
checkParEnd (')':_) 0 = False
checkParEnd (')':xs) i = checkParEnd xs (i - 1)
checkParEnd (_:xs) i = checkParEnd xs i

isSkip :: Char -> Bool
isSkip ' ' = True
isSkip '\n' = True
isSkip '\t' = True
isSkip _ = False

useCompare :: String -> Either String String
useCompare str
    | all isSkip str || checkParEnd str 0 = Right str
    | otherwise = Left $ "[" <> str <> "]" -- les brackets à enlever, c'est pour debug hehe

checkCompare :: Either String String -> IO ()
checkCompare (Right str) = getUserInput str
checkCompare (Left str) = putStrLn str >> getUserInput "" -- ici on passe la string au parsing à la place de putStrLn

getUserInput :: String -> IO ()
getUserInput txt = isEOF >>= \x -> checkEof x
    where
        checkEof :: Bool -> IO ()
        checkEof True = putStrLn "exit" >> exitSuccess
        checkEof _ = getLine >>= \content -> checkCompare $ useCompare $ txt <> content

main :: IO ()
main = getUserInput ""
