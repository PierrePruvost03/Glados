module Main (main) where

import Test.HUnit
import ParserTests
import SExprTests
import AstParsingTest
import qualified System.Exit as Exit

tests :: Test
tests = TestList (parserTests <> sExprTests <> astParsingTests)

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
