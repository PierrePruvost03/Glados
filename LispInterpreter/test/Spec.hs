module Main (main) where

import AstParsingTest
import EvalAstTests
import ParserTests
import PrimitivesTests
import SExprTests
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList (parserTests <> sExprTests <> astParsingTests <> evalAstTests <> primitivesTests)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
