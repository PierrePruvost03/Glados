module EvalAstTests (evalAstTests) where

import Test.HUnit
import Interpreter.EvalAst (evalAst)
import Interpreter.BaseEnv (Value(..), emptyEnv, extendEnv)
import DataStruct.Ast (Ast(..), AstValue(..))

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

evalAstTests :: [Test]
evalAstTests = [
        TestLabel "evalAst AValue" testEvalAstValue,
        TestLabel "evalAst ASymbol found" testEvalAstSymbolFound,
        TestLabel "evalAst ASymbol not found" testEvalAstSymbolNotFound,
        TestLabel "evalAst AIf condition" testEvalAstIfCondition,
        TestLabel "evalAst AList" testEvalAstList,
        TestLabel "evalAst ADefine" testEvalAstDefine
    ]
