module Main (main) where

-- import Lib

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
    content <- getLine
    let result = content
    if null content && compareEnoughPar result
        then putStrLn result
        else getUserInput $ Just result
getUserInput (Just txt) = do
    content <- getLine
    let result = txt ++ "\n" ++ content
    if null content && compareEnoughPar result
        then putStrLn $ "[" ++ result ++ "]"
        else getUserInput $ Just result

main :: IO ()
main = do
    getUserInput Nothing
