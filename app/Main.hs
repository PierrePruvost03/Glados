{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Main (main) where

-- import Lib
import Control.Monad (when, forever)
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
compareEnoughPar txt = countOpenPar txt <= countClosedPar txt

getUserInput :: Maybe String -> IO ()
getUserInput Nothing = do
    done <- isEOF
    when done $ putStrLn "exit" >> exitSuccess
    content <- getLine
    case compareEnoughPar content of
        True -> putStrLn $ "[" ++ content ++ "]"
        False -> getUserInput $ Just content
getUserInput (Just txt) = do
    done <- isEOF
    when done $ putStrLn "exit" >> exitSuccess
    content <- getLine
    case compareEnoughPar $ txt ++ "\n" ++ content of
        True -> putStrLn $ "[" ++ txt ++ "\n" ++ content ++ "]"
        False -> getUserInput $ Just $ txt ++ "\n" ++ content

main :: IO ()
main = forever $ do
    done <- isEOF
    when done $ putStrLn "exit" >> exitSuccess
    getUserInput Nothing
