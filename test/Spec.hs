module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit

testHello :: Test
testHello = TestCase $ assertEqual "should success" "hello" "hello"

tests :: Test
tests = TestList [TestLabel "hello" testHello]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
