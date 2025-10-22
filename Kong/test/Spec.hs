module Main (main) where

import qualified System.Exit as Exit
import Test.HUnit

testKongCompilerExists :: Test
testKongCompilerExists =
  TestCase
    ( assertBool
        "Kong compiler module should exist (placeholder)"
        True
    )

kongTests :: [Test]
kongTests = [
    TestLabel "kong compiler exists" testKongCompilerExists
  ]

tests :: Test
tests = TestList kongTests

main :: IO ()
main = do
  putStrLn "Running Kong Compiler Tests..."
  result <- runTestTT tests
  if failures result > 0 
    then Exit.exitFailure 
    else do
      putStrLn "All Kong tests passed!"
      Exit.exitSuccess
