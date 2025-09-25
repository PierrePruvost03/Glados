module EvalAstTests (evalAstTests) where

import Test.HUnit
import Data.List (isInfixOf)
import Interpreter.EvalAst (evalAst)
import Interpreter.BaseEnv (emptyEnv, extendEnv)
import DataStruct.Value (Value (..))
import DataStruct.Ast (Ast(..), AstValue(..), AstLambda(..))

testEvalAstValue :: Test
testEvalAstValue =
  TestCase (assertEqual "should evaluate AstValue to Value"
    (Right (VInt 42, emptyEnv))
    (evalAst emptyEnv (AValue ((0,0), AstInteger 42))))

testEvalAstSymbolFound :: Test
testEvalAstSymbolFound =
  TestCase (assertEqual "should lookup symbol in environment"
    (Right (VBool True, extendEnv emptyEnv [("x", VBool True)]))
    (evalAst (extendEnv emptyEnv [("x", VBool True)]) (ASymbol ((0,0), "x"))))

testEvalAstSymbolNotFound :: Test
testEvalAstSymbolNotFound =
  TestCase (assertBool "should fail when symbol not in environment"
    (case evalAst emptyEnv (ASymbol ((0,0), "undefined")) of
        Left msg -> "Unbound variable: undefined" == msg
        _ -> False))

testEvalAstIfCondition :: Test
testEvalAstIfCondition =
  TestCase (assertEqual "should evaluate if condition correctly"
    (Right (VInt 10, emptyEnv))
    (evalAst emptyEnv (AIf ((0,0), AValue ((0,0), AstBool True)) 
                          ((0,0), AValue ((0,0), AstInteger 10))
                          ((0,0), AValue ((0,0), AstInteger 20)))))

testEvalAstList :: Test
testEvalAstList =
  TestCase (assertEqual "should evaluate list of values"
    (Right (VList [VInt 1, VBool True, VString "hello"], emptyEnv))
    (evalAst emptyEnv (AList ((0,0), [
        AValue ((0,0), AstInteger 1),
        AValue ((0,0), AstBool True),
        AValue ((0,0), AstString "hello")
      ]))))

testEvalAstDefine :: Test
testEvalAstDefine =
  TestCase (assertEqual "should define variable and update environment"
    (Right (VInt 42, extendEnv emptyEnv [("x", VInt 42)]))
    (evalAst emptyEnv (ADefine ((0,0), "x") ((0,0), AValue ((0,0), AstInteger 42)))))

testEvalAstLambda :: Test
testEvalAstLambda =
  TestCase (assertEqual "should create lambda value"
    (Right (VLambda ["x"] (AValue ((0,0), AstInteger 42)) emptyEnv, emptyEnv))
    (evalAst emptyEnv (ALambdas ((0,0), AstLambda [ASymbol ((0,0), "x")] (AValue ((0,0), AstInteger 42))))))

testEvalAstLambdaInvalidParam :: Test
testEvalAstLambdaInvalidParam =
  TestCase (assertBool "should fail on invalid lambda parameter"
    (case evalAst emptyEnv (ALambdas ((0,0), AstLambda [AValue ((0,0), AstInteger 1)] (AValue ((0,0), AstInteger 42)))) of
        Left msg -> "Invalid parameter:" `isInfixOf` msg
        _ -> False))

testEvalAstCallLambda :: Test
testEvalAstCallLambda =
  TestCase (assertEqual "should call lambda function"
    (Right (VInt 42, extendEnv emptyEnv [("f", VLambda ["x"] (ASymbol ((0,0), "x")) emptyEnv)]))
    (evalAst (extendEnv emptyEnv [("f", VLambda ["x"] (ASymbol ((0,0), "x")) emptyEnv)]) 
             (ACall ((0,0), "f") ((0,0), AList ((0,0), [AValue ((0,0), AstInteger 42)])))))

testEvalAstCallPrimitive :: Test
testEvalAstCallPrimitive =
  TestCase (assertEqual "should call primitive function"
    (Right (VInt 7, extendEnv emptyEnv [("+", VPrim "+" (\arg -> case arg of [VInt a, VInt b] -> Right (VInt (a + b)); _ -> Left "Invalid args"))]))
    (evalAst (extendEnv emptyEnv [("+", VPrim "+" (\arg -> case arg of [VInt a, VInt b] -> Right (VInt (a + b)); _ -> Left "Invalid args"))])
             (ACall ((0,0), "+") ((0,0), AList ((0,0), [AValue ((0,0), AstInteger 3), AValue ((0,0), AstInteger 4)])))))

testEvalAstCallUndefined :: Test
testEvalAstCallUndefined =
  TestCase (assertBool "should fail when calling undefined function"
    (case evalAst emptyEnv (ACall ((0,0), "undefined") ((0,0), AList ((0,0), []))) of
        Left msg -> "Unbound function: undefined" == msg
        _ -> False))

testEvalAstCallWrongArgCount :: Test
testEvalAstCallWrongArgCount =
  TestCase (assertBool "should fail with wrong argument count"
    (case evalAst (extendEnv emptyEnv [("f", VLambda ["x", "y"] (AValue ((0,0), AstInteger 1)) emptyEnv)])
                  (ACall ((0,0), "f") ((0,0), AList ((0,0), [AValue ((0,0), AstInteger 42)]))) of
        Left msg -> "Wrong number of arguments:" `isInfixOf` msg
        _ -> False))

evalAstTests :: [Test]
evalAstTests = [
        TestLabel "evalAst AValue" testEvalAstValue,
        TestLabel "evalAst ASymbol found" testEvalAstSymbolFound,
        TestLabel "evalAst ASymbol not found" testEvalAstSymbolNotFound,
        TestLabel "evalAst AIf condition" testEvalAstIfCondition,
        TestLabel "evalAst AList" testEvalAstList,
        TestLabel "evalAst ADefine" testEvalAstDefine,
        TestLabel "evalAst ALambda" testEvalAstLambda,
        TestLabel "evalAst ALambda invalid param" testEvalAstLambdaInvalidParam,
        TestLabel "evalAst ACall lambda" testEvalAstCallLambda,
        TestLabel "evalAst ACall primitive" testEvalAstCallPrimitive,
        TestLabel "evalAst ACall undefined" testEvalAstCallUndefined,
        TestLabel "evalAst ACall wrong args" testEvalAstCallWrongArgCount
    ]
