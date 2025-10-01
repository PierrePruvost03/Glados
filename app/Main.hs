{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Main (main) where

-- import Lib
import System.IO (isEOF)
import System.Exit (exitSuccess)

isOpenPar :: Char -> Int
isOpenPar '(' = 1
isOpenPar _ = 0

isClosedPar :: Char -> Int
isClosedPar ')' = 1
isClosedPar _ = 0

countOpenPar :: String -> Int
countOpenPar txt = sum (map isOpenPar txt)

countClosedPar :: String -> Int
countClosedPar txt = sum (map isClosedPar txt)

compareEnoughPar :: String -> Bool
compareEnoughPar "" = False
compareEnoughPar txt = countOpenPar txt <= countClosedPar txt

useCompare :: String -> Either String String
useCompare str
    | compareEnoughPar str = Left $ "[" <> str <> "]"
    | otherwise = Right str

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
