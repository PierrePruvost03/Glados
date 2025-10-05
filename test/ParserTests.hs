module ParserTests (parserTests) where

import Parser
import Test.HUnit

testParseCharSuccess :: Test
testParseCharSuccess =
  TestCase
    ( assertEqual
        "should parse 'c'"
        (Right ('c', ("aca", (0, 1))))
        (runParser (parseChar 'c') ("caca", (0, 0)))
    )

testParseCharFail :: Test
testParseCharFail =
  TestCase
    ( assertBool
        "should fail on wrong char"
        ( case runParser (parseChar 'x') ("caca", (0, 0)) of
            Left _ -> True
            _ -> False
        )
    )

testParseStringSuccess :: Test
testParseStringSuccess =
  TestCase
    ( assertEqual
        "should parse string 'hello'"
        (Right ("hello", (" world", (0, 5))))
        (runParser (parseString "hello") ("hello world", (0, 0)))
    )

testParseStringFail :: Test
testParseStringFail =
  TestCase
    ( assertBool
        "should fail parsing wrong string"
        ( case runParser (parseString "bye") ("hello", (0, 0)) of
            Left _ -> True
            _ -> False
        )
    )

testParseInt :: Test
testParseInt =
  TestCase
    ( assertEqual
        "should parse int"
        (Right (123, ("abc", (0, 3))))
        (runParser parseInt ("123abc", (0, 0)))
    )

testParseFloat :: Test
testParseFloat =
  TestCase
    ( assertEqual
        "should parse float"
        (Right (12.34, ("xyz", (0, 5))))
        (runParser parseFloat ("12.34xyz", (0, 0)))
    )

parserTests :: [Test]
parserTests =
  [ TestLabel "parseChar success" testParseCharSuccess,
    TestLabel "parseChar fail" testParseCharFail,
    TestLabel "parseString success" testParseStringSuccess,
    TestLabel "parseString fail" testParseStringFail,
    TestLabel "parseInt" testParseInt,
    TestLabel "parseFloat" testParseFloat
  ]
