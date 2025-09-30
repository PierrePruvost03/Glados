module AstParsingTest (astParsingTests) where

import Test.HUnit
import DataStruct.Ast
import DataStruct.SExpr
import AstParsing

lc0 :: (Int, Int)
lc0 = (0,0)

lc1 :: (Int, Int)
lc1 = (0,1)

lc2 :: (Int, Int)
lc2 = (0,2)

lc3 :: (Int, Int)
lc3 = (0,3)

testParseValueInt :: Test
testParseValueInt =
  TestCase (assertEqual "should parse integer value"
    (Right (AValue (lc0, AstInteger 42)))
    (parseAstFromSExpr (SInt (lc0, 42))))

testParseValueString :: Test
testParseValueString =
    TestCase (assertEqual "should parse string value"
                 (Right (AValue (lc0, AstString "42")))
                 (parseAstFromSExpr (SSymbol (lc0, "\"42\""))))

testParseSymbol :: Test
testParseSymbol =
  TestCase (assertEqual "should parse symbol"
    (Right (ASymbol (lc1, "x")))
    (parseAstFromSExpr (SSymbol (lc1, "x"))))

testParseList :: Test
testParseList =
  TestCase (assertBool "should parse list as error under current call-first semantics"
        (case parseAstFromSExpr lst of
            Left ("call","call head must be a symbol", (0,0)) -> True
            _ -> False))
  where
    lst =
      SList (lc0,
        [ SInt (lc1, 1)
        , SSymbol (lc2, "x")
        , SList (lc3, [SInt (lc1, 2)])
        ])

testParseDefine :: Test
testParseDefine =
  TestCase (assertEqual "should parse define"
        expected
        (parseAstFromSExpr sexpr))
  where
    sexpr = SList (lc0, [ SSymbol (lc0, "define")
                        , SSymbol (lc1, "n")
                        , SInt (lc2, 7)
                        ])
    expected = Right (ADefine { name  = (lc1, "n")
                              , value = (lc0, AValue (lc2, AstInteger 7)) })

testParseLambda :: Test
testParseLambda =
  TestCase (assertEqual "should parse lambda with args and body"
        expected
        (parseAstFromSExpr sexpr))
  where
    sexpr = SList (lc0,
              [ SSymbol (lc0, "lambda")
              , SList (lc1, [ SSymbol (lc1, "x"), SSymbol (lc2, "y") ])
              , SInt (lc3, 10)
              ])
    expected = Right (ALambdas (lc0, AstLambda
                  [ ASymbol (lc1, "x"), ASymbol (lc2, "y") ]
                  (AValue (lc3, AstInteger 10))))

testParseLambdaArgsMalformed :: Test
testParseLambdaArgsMalformed =
  TestCase (assertBool "lambda should fail with non-symbol args"
        (case parseAstFromSExpr sexpr of
            Left ("lambda-args", _, _) -> True
            Left ("lambda", _, _) -> True
            _ -> False))
  where
    sexpr = SList (lc0,
              [ SSymbol (lc0, "lambda")
              , SList (lc1, [ SInt (lc1, 1) ])
              , SInt (lc2, 2)
              ])

testParseIf :: Test
testParseIf =
  TestCase (assertEqual "should parse if expression"
        expected
        (parseAstFromSExpr sexpr))
  where
    sexpr = SList (lc0,
              [ SSymbol (lc0, "if")
              , SSymbol (lc1, "zero?")
              , SInt (lc2, 1)
              , SInt (lc3, 0)
              ])
    expected = Right (AIf { ifCond = (lc0, ASymbol (lc1, "zero?"))
                          , ifThen = (lc0, AValue (lc2, AstInteger 1))
                          , ifElse = (lc0, AValue (lc3, AstInteger 0)) })

testParseIfMalformed :: Test
testParseIfMalformed =
  TestCase (assertBool "if should be malformed when not 3 branches"
        (case parseAstFromSExpr sexpr of
            Left ("if", _, _) -> True
            _ -> False))
  where
    sexpr = SList (lc0, [ SSymbol (lc0, "if"), SInt (lc1, 1) ])

testParseCall :: Test
testParseCall =
  TestCase (assertEqual "should parse call"
        expected
        (parseAstFromSExpr sexpr))
  where
    sexpr = SList (lc0, [ SSymbol (lc1, "*"), SInt (lc2, 2), SInt (lc3, 3) ])
    expectedArgs = AList (lc0, [ AValue (lc2, AstInteger 2)
                               , AValue (lc3, AstInteger 3) ])
    expected = Right (ACall { name = (lc1, "*"), args = (lc0, expectedArgs) })

testParseCallHeadMalformed :: Test
testParseCallHeadMalformed =
  TestCase (assertBool "call should fail when head is not a symbol"
        (case parseAstFromSExpr sexpr of
            Left ("call", _, _) -> True
            _ -> False))
  where
    sexpr = SList (lc0, [ SInt (lc1, 1), SInt (lc2, 2) ])

testParsePlainListNotCall :: Test
testParsePlainListNotCall =
  TestCase (assertBool "plain list (non-symbol head) should be rejected as call"
        (case parseAstFromSExpr sexpr of
            Left ("call", _, _) -> True
            _ -> False))
  where
    sexpr = SList (lc0, [ SList (lc1, []), SInt (lc2, 1) ])

testParseEmptyList :: Test
testParseEmptyList =
  TestCase (assertBool "should reject empty list as empty application"
    (case parseAstFromSExpr (SList (lc0, [])) of
        Left ("call","empty list application",(0,0)) -> True
        _ -> False))

astParsingTests :: [Test]
astParsingTests =
  [ TestLabel "value int"                testParseValueInt
  , TestLabel "value string"             testParseValueString
  , TestLabel "symbol"                   testParseSymbol
  , TestLabel "list"                     testParseList
  , TestLabel "define"                   testParseDefine
  , TestLabel "lambda"                   testParseLambda
  , TestLabel "lambda args malformed"    testParseLambdaArgsMalformed
  , TestLabel "if"                       testParseIf
  , TestLabel "if malformed"             testParseIfMalformed
  , TestLabel "call"                     testParseCall
  , TestLabel "call head malformed"      testParseCallHeadMalformed
  , TestLabel "plain list not call"      testParsePlainListNotCall
  , TestLabel "empty list"               testParseEmptyList
  ]