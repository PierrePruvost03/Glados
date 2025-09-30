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
    let result = content
    if compareEnoughPar result
        then putStrLn $ "[" ++ result ++ "]"
        else getUserInput $ Just result
getUserInput (Just txt) = do
    done <- isEOF
    when done $ putStrLn "exit" >> exitSuccess
    content <- getLine
    let result = txt ++ "\n" ++ content
    if compareEnoughPar result
        then putStrLn $ "[" ++ result ++ "]"
        else getUserInput $ Just result

main :: IO ()
main = forever $ do
    done <- isEOF
    when done $ putStrLn "exit" >> exitSuccess
    getUserInput Nothing
