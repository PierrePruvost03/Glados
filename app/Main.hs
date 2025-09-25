module Main (main) where

import Lib

main :: IO ()
main = do
    content <- getContents
    putStrLn $ "[" ++ content ++ "]"
    someFunc