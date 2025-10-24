module KongCompilerTests (kongCompilerTests) where

import Data.List (isInfixOf)
import Test.HUnit

import Compiler.KongCompiler
import DataStruct.Ast
import DataStruct.VM
import DataStruct.Bytecode.Value
import DataStruct.Bytecode.Number
import DataStruct.Bytecode.Op

testCompileInteger :: Test
testCompileInteger =
  TestCase
    ( assertEqual
        "should compile integer value to Push VInt instruction"
        (Right [Push (VInt 42)])
        (compile (AExpress (AValue (ANumber (AInteger 42)))))
    )

testCompileFloat :: Test
testCompileFloat =
  TestCase $ do
    let result = compile (AExpress (AValue (ANumber (AFloat 3.14))))
    case result of
      Right [Push (VFloat f)] -> assertBool "Should compile float value" (abs (f - 3.14) < 0.001)
      Right other -> assertFailure $ "Unexpected result: " ++ show other
      Left err -> assertFailure $ "Should not fail: " ++ show err

testCompileBool :: Test
testCompileBool =
  TestCase
    ( assertEqual
        "should compile boolean value to Push VBool instruction"
        (Right [Push (VBool True)])
        (compile (AExpress (AValue (ANumber (ABool True)))))
    )

testCompileString :: Test
testCompileString =
  TestCase
    ( assertEqual
        "should compile string value to Push VString instruction"
        (Right [Push (VList ("hello"))])
        (compile (AExpress (AValue (AString "hello"))))
    )

testCompileChar :: Test
testCompileChar =
  TestCase
    ( assertEqual
        "should compile char value to Push VChar instruction"
        (Right [Push (VNumber (VChar 'a'))])
        (compile (AExpress (AValue (ANumber (AChar 'a')))))
    )

testCompileVarDecl :: Test
testCompileVarDecl =
  TestCase
    ( assertEqual
        "should compile variable declaration to default value push and SetVar"
        (Right [Push (VInt 0), SetVar "x"])
        (compile (AVarDecl TInt "x" Nothing))
    )

testCompileVarDeclWithValue :: Test
testCompileVarDeclWithValue =
  TestCase
    ( assertEqual
        "should compile variable declaration with value"
        (Right [Push (VInt 42), SetVar "x"])
        (compile (AVarDecl TInt "x" (Just (AValue (ANumber (AInteger 42))))))
    )

testCompileVarCall :: Test
testCompileVarCall =
  TestCase
    ( assertEqual
        "should compile variable reference to PushEnv"
        (Right [PushEnv "x"])
        (compile (AExpress (AValue (AVarCall "x"))))
    )

testCompileAttribution :: Test
testCompileAttribution =
  TestCase
    ( assertEqual
        "should compile variable attribution"
        (Right [Push (VInt 10), SetVar "x"])
        (compile (AExpress (AAttribution "x" (AValue (ANumber (AInteger 10))))))
    )

testCompileFunctionCall :: Test
testCompileFunctionCall =
  TestCase
    ( assertEqual
        "should compile function call with arguments in reverse order"
        (Right [Push (VInt 2), Push (VInt 1), PushEnv "myFunc", Call])
        (compile (AExpress (ACall "myFunc" [AValue (ANumber (AInteger 1)), AValue (ANumber (AInteger 2))])))
    )

testCompileBuiltinAdd :: Test
testCompileBuiltinAdd =
  TestCase
    ( assertEqual
        "should compile builtin addition operation"
        (Right [Push (VInt 2), Push (VInt 1), DoOp Add])
        (compile (AExpress (ACall "+" [AValue (ANumber (AInteger 1)), AValue (ANumber (AInteger 2))])))
    )

testCompileBuiltinSub :: Test
testCompileBuiltinSub =
  TestCase
    ( assertEqual
        "should compile builtin subtraction operation"
        (Right [Push (VInt 3), Push (VInt 5), DoOp Sub])
        (compile (AExpress (ACall "-" [AValue (ANumber (AInteger 5)), AValue (ANumber (AInteger 3))])))
    )

testCompileBuiltinEqual :: Test
testCompileBuiltinEqual =
  TestCase
    ( assertEqual
        "should compile builtin equality operation"
        (Right [Push (VInt 2), Push (VInt 1), DoOp Equal])
        (compile (AExpress (ACall "==" [AValue (ANumber (AInteger 1)), AValue (ANumber (AInteger 2))])))
    )

testCompileSymbol :: Test
testCompileSymbol =
  TestCase
    ( assertEqual
        "should compile symbol to PushEnv"
        (Right [PushEnv "mySymbol"])
        (compile (ASymbol "mySymbol"))
    )

testCompileReturn :: Test
testCompileReturn =
  TestCase
    ( assertEqual
        "should compile return statement"
        (Right [Push (VInt 42), Ret])
        (compile (AReturn (AExpress (AValue (ANumber (AInteger 42))))))
    )

testCompileBlock :: Test
testCompileBlock =
  TestCase
    ( assertEqual
        "should compile block of statements"
        (Right [Push (VInt 5), SetVar "x", PushEnv "x"])
        (compile (ABlock [
          AVarDecl TInt "x" (Just (AValue (ANumber (AInteger 5)))),
          AExpress (AValue (AVarCall "x"))
        ]))
    )

testUnsupportedAst :: Test
testUnsupportedAst =
  TestCase
    ( assertBool
        "should fail with UnsupportedAst error"
        ( case compile (AInclude "someModule" ["item1", "item2"]) of
            Left (UnsupportedAst msg) -> "AInclude" `isInfixOf` msg
            _ -> False
        )
    )

testUnsupportedValue :: Test
testUnsupportedValue =
  TestCase
    ( assertBool
        "should fail with unsupported value type"
        ( case compile (AExpress (AValue (ATuple []))) of
            Left (UnsupportedAst msg) -> "Unsupported value" `isInfixOf` msg
            _ -> False
        )
    )

kongCompilerTests :: [Test]
kongCompilerTests =
  [
    TestLabel "compile integer" testCompileInteger,
    TestLabel "compile float" testCompileFloat,
    TestLabel "compile boolean" testCompileBool,
    TestLabel "compile string" testCompileString,
    TestLabel "compile char" testCompileChar,
    TestLabel "compile var declaration" testCompileVarDecl,
    TestLabel "compile var declaration with value" testCompileVarDeclWithValue,
    TestLabel "compile variable call" testCompileVarCall,
    TestLabel "compile attribution" testCompileAttribution,
    TestLabel "compile function call" testCompileFunctionCall,
    TestLabel "compile builtin add" testCompileBuiltinAdd,
    TestLabel "compile builtin sub" testCompileBuiltinSub,
    TestLabel "compile builtin equal" testCompileBuiltinEqual,
    TestLabel "compile symbol" testCompileSymbol,
    TestLabel "compile return" testCompileReturn,
    TestLabel "compile block" testCompileBlock,
    TestLabel "unsupported ast" testUnsupportedAst,
    TestLabel "unsupported value" testUnsupportedValue
  ]
