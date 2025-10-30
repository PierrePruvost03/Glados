module Main (main) where

import qualified System.Exit as Exit
import Test.HUnit

import KongCompilerTests (kongCompilerTests)
import KongVMTests (kongVMTests)
import BaseParsingTest(baseParsingTests)

tests :: Test
tests = TestList (kongCompilerTests <> kongVMTests <> baseParsingTests)

main :: IO ()
main = do
  putStrLn "Running Kong Compiler Tests..."
  result <- runTestTT tests
  if failures result > 0
    then Exit.exitFailure
    else do
      putStrLn "All Kong tests passed!"
      Exit.exitSuccess
