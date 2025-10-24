module AstParsing.ErrorMessage (printParsingResult) where

import DataStruct.Ast
import Parser

getLineAt :: [String] -> Int -> String
getLineAt ls i
  | i < length ls = ls !! i
  | otherwise = ""

printError :: String -> (Int, Int) -> String
printError str (l, c) = unlines [lc, cutLine, arrow]
  where
    line = getLineAt (lines str) l
    (start, end) = getBounds line c
    cutLine = extractLine line start end
    arrow = replicate (calculateArrowPos cutLine c start) ' ' ++ "^"
    lc = "at line " ++ show (l + 1) ++ " columns " ++ show (c + 1)

getBounds :: String -> Int -> (Int, Int)
getBounds line c = (max 0 (c - 40), min (length line) (c + 40))

extractLine :: String -> Int -> Int -> String
extractLine line start end =
  take (end - start) (drop start line)

calculateArrowPos :: String -> Int -> Int -> Int
calculateArrowPos line c start =
  max 0 (min (length line) (c - start))

printParsingResult :: String -> Parser [Ast] -> Either [Ast] String
printParsingResult str p =
  case runParser p (str, (0, 0)) of
    Left (_, context, error_message, lc) -> Right (
        "Error while parsing \n["
        <> context <> "]:" <> error_message <> " "
        <> printError str lc
        )
    Right (res, rest) -> Left res