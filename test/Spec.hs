module Main (main) where

import Test.HUnit
import ParserTests
import SExprTests
import EvalAstTests
import PrimitivesTests
import DataStruct.SExpr
import qualified System.Exit as Exit
import AstParsingTest

tests :: Test
tests = TestList (parserTests <> sExprTests <> astParsingTests <> evalAstTests <> primitivesTests)

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
