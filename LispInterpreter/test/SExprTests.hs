module SExprTests (sExprTests) where

import SExprParser
import Parser
import DataStruct.SExpr
import Test.HUnit

testParseIntSuccess :: Test
testParseIntSuccess =
  TestCase (assertEqual "should parse 32"
    (Right (SInt ((0,0), 32), (" prout", (0,2))))
    (runParser (parseLispInt) ("32 prout", (0,0))))

testParseSymbolSuccess :: Test
testParseSymbolSuccess =
    TestCase (assertEqual "should parse \"prout\""
        (Right (SSymbol ((0,0), "prout"), (" prout", (0,5))))
        (runParser (parseLispSymbol) ("prout prout", (0,0))))

testParseListSuccess :: Test
testParseListSuccess =
    TestCase (assertEqual "should parse \"(prout 42)\""
        (Right (SList ((0,0), [SSymbol ((0,1), "prout"), SInt ((0,7), 42)]), ("", (0,10))))
        (runParser (parseLispList) ("(prout 42)", (0,0))))



sExprTests :: [Test]
sExprTests = [
        TestLabel "parseInt Success" testParseIntSuccess,
        TestLabel "parseSymbol Success" testParseSymbolSuccess,
        TestLabel "parseList Success" testParseListSuccess
    ]
