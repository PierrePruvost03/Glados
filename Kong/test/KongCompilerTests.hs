module KongCompilerTests (kongCompilerTests) where

import Data.List (isInfixOf)
import qualified Data.Vector as V
import qualified Data.Map as M
import Test.HUnit
import Compiler.BytecodeGen.Program.Program (compileWithEnv)
import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Number (Number(..), NumberType(..))
import DataStruct.Bytecode.Op (Op(..))
import DataStruct.Bytecode.Syscall (Syscall(..))
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (emptyEnv, CompilerEnv(..))
import TestHelpers

testCompileInteger :: Test
testCompileInteger =
  TestCase
    ( assertEqual
        "should compile integer value to Push VInt instruction"
        (Right [Push (VNumber (VInt 42))])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))))
    )

testCompileFloat :: Test
testCompileFloat =
  TestCase $ do
    let result = compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (AFloat 3.14)))))))
    case result of
      Right [Push (VNumber (VFloat f))] -> assertBool "Should compile float value" (abs (f - 3.14) < 0.001)
      Right other -> assertFailure $ "Unexpected result: " ++ show other
      Left err -> assertFailure $ "Should not fail: " ++ show err

testCompileBool :: Test
testCompileBool =
  TestCase
    ( assertEqual
        "should compile boolean value to Push VBool instruction"
        (Right [Push (VNumber (VBool True))])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (ABool True))))))))
    )

testCompileString :: Test
testCompileString =
  TestCase
    ( assertEqual
        "should compile string value to Push VString instruction"
        (Right [Push (VList (V.fromList [VNumber (VChar 'h'), VNumber (VChar 'e'), VNumber (VChar 'l'), VNumber (VChar 'l'), VNumber (VChar 'o')]))])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AString "hello")))))))
    )

testCompileArray :: Test
testCompileArray =
  TestCase
    ( assertEqual
        "should compile array literal to CreateList (reversed order)"
        (Right [Push (VNumber (VInt 3)), Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), CreateList 3])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AArray [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))])))))))
    )

testCompileVector :: Test
testCompileVector =
  TestCase
    ( assertEqual
        "should compile vector literal to CreateList (reversed order)"
        (Right [Push (VNumber (VInt 6)), Push (VNumber (VInt 5)), CreateList 2])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AVector [wrapExpr (AValue (wrapValue (ANumber (AInteger 5)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 6))))])))))))
    )

testCompileTuple :: Test
testCompileTuple =
  TestCase
    ( assertEqual
        "should compile tuple literal to CreateList (reversed order)"
        (Right [Push (VNumber (VInt 8)), Push (VNumber (VInt 7)), CreateList 2])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ATuple [wrapExpr (AValue (wrapValue (ANumber (AInteger 7)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 8))))])))))))
    )

testCompileStruct :: Test
testCompileStruct =
  TestCase
    ( assertEqual
        "should compile struct literal to CreateStruct"
        (Right [Push (VNumber (VInt 42)), Push (VList (V.fromList [VNumber (VChar 'a')])), CreateStruct ["name", "age"]])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AStruct [ ("name", wrapExpr (AValue (wrapValue (AString "a"))))
                                           , ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 42)))))
                                           ])))))))
    )

testStructWithHeapString :: Test
testStructWithHeapString =
  TestCase
  ( assertEqual
    "should heapify string then load it when used in struct"
    (Right
      [ Push (VList (V.fromList [VNumber (VChar 'p'), VNumber (VChar 'i'), VNumber (VChar 'p'), VNumber (VChar 'i')]))
      , Alloc
      , StoreRef
      , SetVar "x"
      , Push (VNumber (VInt 42))
      , PushEnv "x"
      , LoadRef
      , CreateStruct ["name", "age"]
      , Alloc
      , StoreRef
      , SetVar "a"
      ])
    ( compileWithEnv emptyEnv
      ( wrapAst (ABlock
        [ wrapAst (AVarDecl (wrapType TString) "x" (Just (wrapExpr (AValue (wrapValue (AString "pipi"))))))
        , wrapAst (AVarDecl
          (wrapType (TStruct "Person"))
          "a"
          ( Just
            ( wrapExpr (AValue
              ( wrapValue (AStruct
                [ ("name", wrapExpr (AValue (wrapValue (AVarCall "x"))))
                , ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 42)))))
                ]
              )))
            )
          ))
        ])
      )
    )
  )

testArrayDeclarationAndAccess :: Test
testArrayDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile array declaration and access"
    (Right
      [ Push (VNumber (VInt 3))
      , Push (VNumber (VInt 2))
      , Push (VNumber (VInt 1))
      , CreateList 3
      , Alloc
      , StoreRef
      , SetVar "arr"
      , Push (VNumber (VInt 1))
      , PushEnv "arr"
      , LoadRef
      , GetList
      ])
    ( compileWithEnv emptyEnv
      ( wrapAst (ABlock
                [ wrapAst (AVarDecl (wrapType (TArray (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))))) "arr"
          (Just (wrapExpr (AValue (wrapValue (AArray [ wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))
                       , wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))
                       , wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
                       ]))))))
        , wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AArrayAccess (wrapExpr (AValue (wrapValue (AVarCall "arr")))) (wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))))))))
        ])
      )
    )
  )

testArrayReassignment :: Test
testArrayReassignment =
  TestCase
  ( assertEqual
    "should compile array reassignment"
    (Right
      [ Push (VNumber (VInt 3))
      , Push (VNumber (VInt 2))
      , Push (VNumber (VInt 1))
      , CreateList 3
      , Alloc
      , StoreRef
      , SetVar "arr"
      , Push (VNumber (VInt 6))
      , Push (VNumber (VInt 5))
      , Push (VNumber (VInt 4))
      , CreateList 3
      , PushEnv "arr"
      , StoreRef
      ])
    ( compileWithEnv emptyEnv
      ( wrapAst (ABlock
                [ wrapAst (AVarDecl (wrapType (TArray (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))))) "arr"
          (Just (wrapExpr (AValue (wrapValue (AArray [ wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))
                       , wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))
                       , wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
                       ]))))))
        , wrapAst (AExpress
          (wrapExpr (AAttribution "arr"
            (wrapExpr (AValue (wrapValue (AArray [ wrapExpr (AValue (wrapValue (ANumber (AInteger 4))))
                     , wrapExpr (AValue (wrapValue (ANumber (AInteger 5))))
                     , wrapExpr (AValue (wrapValue (ANumber (AInteger 6))))
                     ])))))))
        ])
      )
    )
  )

testStructDeclarationAndAccess :: Test
testStructDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile struct declaration and access"
    (Right
      [ Push (VNumber (VInt 30))
      , Push (VList (V.fromList [VNumber (VChar 'A'), VNumber (VChar 'd'), VNumber (VChar 'a')]))
      , CreateStruct ["name", "age"]
      , Alloc
      , StoreRef
      , SetVar "person"
      , PushEnv "person"
      , LoadRef
      , GetStruct "name"
      ])
    ( compileWithEnv (emptyEnv { structDefs = M.fromList [("Person", [(wrapType TString, "name"), (wrapType TInt, "age")])] })
      ( wrapAst (ABlock
        [ wrapAst (AVarDecl (wrapType (TStruct "Person")) "person"
          (Just (wrapExpr (AValue (wrapValue (AStruct [ ("name", wrapExpr (AValue (wrapValue (AString "Ada"))))
                      , ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 30)))))
                      ]))))))
        , wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AStructAccess (wrapExpr (AValue (wrapValue (AVarCall "person")))) ["name"])))))
        ])
      )
    )
  )

testStructReassignment :: Test
testStructReassignment =
  TestCase
    ( assertBool
        "should fail struct reassignment due to type inference issues"
        ( case compileWithEnv emptyEnv
          ( wrapAst (ABlock
            [ wrapAst (AVarDecl (wrapType (TStruct "Person")) "person"
              (Just (wrapExpr (AValue (wrapValue (AStruct [ ("name", wrapExpr (AValue (wrapValue (AString "Ada"))))
                          , ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 30)))))
                          ]))))))
            , wrapAst (AExpress
              (wrapExpr (AAttribution "person"
                (wrapExpr (AValue (wrapValue (AStruct [ ("name", wrapExpr (AValue (wrapValue (AString "Bob"))))
                         , ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 31)))))
                         ])))))))
            ])
          ) of
            Left (IllegalAssignment _ _) -> True
            _ -> False
        )
    )

testVectorDeclarationAndAccess :: Test
testVectorDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile vector declaration and access"
    (Right
      [ Push (VNumber (VInt 20))
      , Push (VNumber (VInt 10))
      , CreateList 2
      , Alloc
      , StoreRef
      , SetVar "vec"
      , Push (VNumber (VInt 0))
      , PushEnv "vec"
      , LoadRef
      , GetList
      ])
    ( compileWithEnv emptyEnv
      ( wrapAst (ABlock
                [ wrapAst (AVarDecl (wrapType (TVector (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))))) "vec"
          (Just (wrapExpr (AValue (wrapValue (AVector [ wrapExpr (AValue (wrapValue (ANumber (AInteger 10))))
                       , wrapExpr (AValue (wrapValue (ANumber (AInteger 20))))
                       ]))))))
        , wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AVectorAccess (wrapExpr (AValue (wrapValue (AVarCall "vec")))) (wrapExpr (AValue (wrapValue (ANumber (AInteger 0))))))))))
        ])
      )
    )
  )

testTupleDeclarationAndAccess :: Test
testTupleDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile tuple declaration and access"
    (Right
      [ Push (VNumber (VInt 9))
      , Push (VNumber (VInt 5))
      , CreateList 2
      , Alloc
      , StoreRef
      , SetVar "pair"
      , Push (VNumber (VInt 0))
      , PushEnv "pair"
      , LoadRef
      , GetList
      ])
    ( compileWithEnv emptyEnv
      ( wrapAst (ABlock
        [ wrapAst (AVarDecl (wrapType (TTuple [wrapType TInt, wrapType TInt])) "pair"
          (Just (wrapExpr (AValue (wrapValue (ATuple [ wrapExpr (AValue (wrapValue (ANumber (AInteger 5))))
                      , wrapExpr (AValue (wrapValue (ANumber (AInteger 9))))
                      ]))))))
        , wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (ATupleAccess (wrapExpr (AValue (wrapValue (AVarCall "pair")))) (wrapExpr (AValue (wrapValue (ANumber (AInteger 0))))))))))
        ])
      )
    )
  )

testCompileChar :: Test
testCompileChar =
  TestCase
    ( assertEqual
        "should compile char value to Push (VNumber (VChar 'a'))"
        (Right [Push (VNumber (VChar 'a'))])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (AChar 'a'))))))))
    )

testCompileVarDecl :: Test
testCompileVarDecl =
  TestCase
  ( assertEqual
    "should reject uninitialized variable declaration"
    (Left (UninitializedVariable "Variable 'x' must be initialized at declaration" (0,0)))
    (compileWithEnv emptyEnv (wrapAst (AVarDecl (wrapType TInt) "x" Nothing)))
  )

testCompileVarDeclWithValue :: Test
testCompileVarDeclWithValue =
  TestCase
  ( assertEqual
    "should compile mutable variable declaration with value to heap"
    (Right [Push (VNumber (VInt 42)), Alloc, StoreRef, SetVar "x"])
    (compileWithEnv emptyEnv (wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42)))))))))
  )

testCompileVarCall :: Test
testCompileVarCall =
  TestCase
    ( assertBool
        "should fail for undeclared variable"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AVarCall "x")))))) of
            Left (UnknownVariable "x" _) -> True
            _ -> False
        )
    )

testCompileAttribution :: Test
testCompileAttribution =
  TestCase
    ( assertBool
        "should fail attribution for undeclared variable"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AAttribution "x" (wrapExpr (AValue (wrapValue (ANumber (AInteger 10))))))))) of
            Left (UnknownVariable "x" _) -> True
            _ -> False
        )
    )

testCompileFunctionCall :: Test
testCompileFunctionCall =
  TestCase
    ( assertBool
        "should fail for undeclared function"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "myFunc")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))])))) of
            Left (UnknownFunction "myFunc" _) -> True
            _ -> False
        )
    )

testCompileBuiltinAdd :: Test
testCompileBuiltinAdd =
  TestCase
    ( assertEqual
        "should compile builtin addition operation"
        (Right [Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), DoOp Add])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "+")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))])))))
    )

testCompileBuiltinSub :: Test
testCompileBuiltinSub =
  TestCase
    ( assertEqual
        "should compile builtin subtraction operation"
        (Right [Push (VNumber (VInt 3)), Push (VNumber (VInt 5)), DoOp Sub])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "-")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 5)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))])))))
    )

testCompileBuiltinEqual :: Test
testCompileBuiltinEqual =
  TestCase
    ( assertEqual
        "should compile builtin equality operation"
        (Right [Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), DoOp Equal])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "==")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))])))))
    )

testCompileSymbol :: Test
testCompileSymbol =
  TestCase
    ( assertBool
        "should fail for undeclared symbol"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AVarCall "mySymbol")))))) of
            Left (UnknownVariable "mySymbol" _) -> True
            _ -> False
        )
    )

testCompileReturn :: Test
testCompileReturn =
  TestCase
    ( assertEqual
        "should compile return statement"
        (Right [Push (VNumber (VInt 42)), Ret])
        (compileWithEnv emptyEnv (wrapAst (AReturn (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))))))
    )

testCompileBlock :: Test
testCompileBlock =
  TestCase
    ( assertEqual
        "should compile block of statements"
        (Right [Push (VNumber (VInt 5)), Alloc, StoreRef, SetVar "x", PushEnv "x", LoadRef])
        (compileWithEnv emptyEnv (wrapAst (ABlock [
          wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 5))))))),
          wrapAst (AExpress (wrapExpr (AValue (wrapValue (AVarCall "x")))))
        ])))
    )

testUnsupportedAst :: Test
testUnsupportedAst =
  TestCase
    ( assertBool
        "should fail with UnsupportedAst error for unsupported nodes like ATypeAlias"
        ( case compileWithEnv emptyEnv (wrapAst (ATypeAlias "MyType" (wrapType TInt))) of
            Left (UnsupportedAst msg _) -> "ATypeAlias" `isInfixOf` msg
            _ -> False
        )
    )

testCompileBuiltinMul :: Test
testCompileBuiltinMul =
  TestCase
    ( assertEqual
        "should compile builtin multiplication operation"
        (Right [Push (VNumber (VInt 4)), Push (VNumber (VInt 3)), DoOp Mul])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "*")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 3)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 4))))])))))
    )

testCompileBuiltinDiv :: Test
testCompileBuiltinDiv =
  TestCase
    ( assertEqual
        "should compile builtin division operation"
        (Right [Push (VNumber (VInt 3)), Push (VNumber (VInt 10)), DoOp Div])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "/")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 10)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))])))))
    )

testCompileBuiltinLessOrEqual :: Test
testCompileBuiltinLessOrEqual =
  TestCase
    ( assertEqual
        "should compile builtin less or equal comparison"
        (Right [Push (VNumber (VInt 5)), Push (VNumber (VInt 3)), DoOp Le])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "<=")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 3)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 5))))])))))
    )

testCompileBuiltinLessThan :: Test
testCompileBuiltinLessThan =
  TestCase
    ( assertEqual
        "should compile builtin less than comparison"
        (Right [Push (VNumber (VInt 5)), Push (VNumber (VInt 3)), DoOp Lt])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "<")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 3)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 5))))])))))
    )

testCompileBuiltinGreaterThan :: Test
testCompileBuiltinGreaterThan =
  TestCase
    ( assertEqual
        "should compile builtin greater than comparison"
        (Right [Push (VNumber (VInt 3)), Push (VNumber (VInt 5)), DoOp Gt])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall ">")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 5)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))])))))
    )

testCompileBuiltinNotEqual :: Test
testCompileBuiltinNotEqual =
  TestCase
    ( assertEqual
        "should compile builtin not equal comparison"
        (Right [Push (VNumber (VInt 5)), Push (VNumber (VInt 3)), DoOp Ne])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "!=")))) [wrapExpr (AValue (wrapValue (ANumber (AInteger 3)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 5))))])))))
    )

testEmptyArray :: Test
testEmptyArray =
  TestCase
    ( assertEqual
        "should compile empty array literal"
        (Right [CreateList 0])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AArray [])))))))
    )

testEmptyStruct :: Test
testEmptyStruct =
  TestCase
    ( assertEqual
        "should compile empty struct literal"
        (Right [CreateStruct []])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AStruct [])))))))
    )

testNestedArrays :: Test
testNestedArrays =
  TestCase
    ( assertEqual
        "should compile nested array literals (reversed order)"
        (Right [Push (VNumber (VInt 4)), Push (VNumber (VInt 3)), CreateList 2, Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), CreateList 2, CreateList 2])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AArray [
          wrapExpr (AValue (wrapValue (AArray [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))]))),
          wrapExpr (AValue (wrapValue (AArray [wrapExpr (AValue (wrapValue (ANumber (AInteger 3)))), wrapExpr (AValue (wrapValue (ANumber (AInteger 4))))])))
        ])))))))
    )

testMultipleVarDeclarations :: Test
testMultipleVarDeclarations =
  TestCase
    ( assertEqual
        "should compile multiple variable declarations in block"
        (Right [
          Push (VNumber (VInt 10)), Alloc, StoreRef, SetVar "x",
          Push (VNumber (VInt 20)), Alloc, StoreRef, SetVar "y",
          PushEnv "y", LoadRef, PushEnv "x", LoadRef, DoOp Add
        ])
        (compileWithEnv emptyEnv (wrapAst (ABlock [
          wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 10))))))),
          wrapAst (AVarDecl (wrapType TInt) "y" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 20))))))),
          wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "+")))) [
            wrapExpr (AValue (wrapValue (AVarCall "x"))),
            wrapExpr (AValue (wrapValue (AVarCall "y")))
          ])))
        ])))
    )

testKonstVariable :: Test
testKonstVariable =
  TestCase
    ( assertEqual
        "should compile konst variable without heap allocation"
        (Right [Push (VNumber (VInt 42)), SetVar "x", PushEnv "x"])
        (compileWithEnv emptyEnv (wrapAst (ABlock [
          wrapAst (AVarDecl (wrapType (TKonst (wrapType TInt))) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))),
          wrapAst (AExpress (wrapExpr (AValue (wrapValue (AVarCall "x")))))
        ])))
    )

testStructFieldAccess :: Test
testStructFieldAccess =
  TestCase
    ( assertBool
        "should fail struct field access without type information"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AStructAccess 
          (wrapExpr (AValue (wrapValue (AStruct [
            ("name", wrapExpr (AValue (wrapValue (AString "Joe")))),
            ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 42)))))
          ]))))
          ["name"]
        )))))) of
            Left (InvalidArguments _ _) -> True
            _ -> False
        )
    )

testOutOfBoundsArrayAccessCompiles :: Test
testOutOfBoundsArrayAccessCompiles =
  TestCase
    ( assertBool
        "should detect out of bounds array access at compile time (with constant index)"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TArray (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))))) "arr"
              (Just (wrapExpr (AValue (wrapValue (AArray [
                wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))
              ])))))),
            wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AArrayAccess 
              (wrapExpr (AValue (wrapValue (AVarCall "arr"))))
              (wrapExpr (AValue (wrapValue (ANumber (AInteger 10)))))
            )))))
          ])) of
            Right _ -> False
            Left (IndexOutOfBounds _ _ _) -> True
            Left _ -> False
        )
    )

testMixedTypeComparison :: Test
testMixedTypeComparison =
  TestCase
    ( assertBool
        "should compile comparison of int and float (numeric compatible)"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "=="))))
          [
            wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
            wrapExpr (AValue (wrapValue (ANumber (AFloat 1.0))))
          ]
        )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testCastIntToFloat :: Test
testCastIntToFloat =
  TestCase
    ( assertBool
        "should compile cast from int to float"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACast (wrapType TFloat) (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))))) of
            Right [Push (VNumber (VInt 42)), Cast NTFloat] -> True
            _ -> False
        )
    )

testCastFloatToInt :: Test
testCastFloatToInt =
  TestCase
    ( assertBool
        "should compile cast from float to int"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACast (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AFloat 3.14))))))))) of
            Right instrs -> case instrs of
                [Push (VNumber (VFloat _)), Cast NTInt] -> True
                _ -> False
            _ -> False
        )
    )

testEmptyBlock :: Test
testEmptyBlock =
  TestCase
    ( assertEqual
        "should compile empty block"
        (Right [])
        (compileWithEnv emptyEnv (wrapAst (ABlock [])))
    )

testReturnWithExpression :: Test
testReturnWithExpression =
  TestCase
    ( assertEqual
        "should compile return with complex expression"
        (Right [Push (VNumber (VInt 3)), Push (VNumber (VInt 2)), DoOp Add, Ret])
        (compileWithEnv emptyEnv (wrapAst (AReturn (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "+"))))
          [
            wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))),
            wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
          ]
        )))))))
    )

testStructDefinition :: Test
testStructDefinition =
  TestCase
    ( assertEqual
        "should compile struct definition"
        (Right [])
        (compileWithEnv emptyEnv (wrapAst (AStruktDef "Person" [
          (wrapType TString, "name"),
          (wrapType TInt, "age")
        ])))
    )

testPrintString :: Test
testPrintString =
  TestCase
    ( assertEqual
        "should compile print with string"
        (Right [Push (VList (V.fromList [VNumber (VChar 'h'), VNumber (VChar 'i')])), Syscall (Print 1)])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "print"))))
          [wrapExpr (AValue (wrapValue (AString "hi")))]
        )))))
    )

testPrintMultipleArgs :: Test
testPrintMultipleArgs =
  TestCase
    ( assertEqual
        "should compile print with multiple arguments"
        (Right [Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), Syscall (Print 2)])
        (compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "print"))))
          [
            wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
            wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))
          ]
        )))))
    )

testStringConcat :: Test
testStringConcat =
  TestCase
    ( assertBool
        "should compile string operations"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType TString) "s1" (Just (wrapExpr (AValue (wrapValue (AString "hello")))))),
            wrapAst (AVarDecl (wrapType TString) "s2" (Just (wrapExpr (AValue (wrapValue (AString "world")))))),
            wrapAst (AExpress (wrapExpr (AValue (wrapValue (AVarCall "s1")))))
          ])) of
            Right _ -> True
            Left _ -> False
        )
    )

testIfElseCompile :: Test
testIfElseCompile =
  TestCase
    ( assertBool
        "should compile if-else statement"
        ( case compileWithEnv emptyEnv (wrapAst (AIf 
            (wrapAst (AExpress (wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "<"))))
              [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
               wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))]))))
            (wrapAst (AReturn (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))))))))
            (Just (wrapAst (AReturn (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ANumber (AInteger 0))))))))))
          )) of
            Right _ -> True
            Left _ -> False
        )
    )

testComplexArithmetic :: Test
testComplexArithmetic =
  TestCase
    ( assertBool
        "should compile complex arithmetic expression"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall "+"))))
            [
              wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "*"))))
                [wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))),
                 wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))]),
              wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "/"))))
                [wrapExpr (AValue (wrapValue (ANumber (AInteger 10)))),
                 wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))])
            ]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testNestedFunctionCalls :: Test
testNestedFunctionCalls =
  TestCase
    ( assertBool
        "should compile nested function calls"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall "print"))))
            [wrapExpr (ACall (wrapExpr (AValue (wrapValue (AVarCall "+"))))
              [wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
               wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))])]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testMixedIntFloat :: Test
testMixedIntFloat =
  TestCase
    ( assertBool
        "should allow mixed int and float operations"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall "+"))))
            [
              wrapExpr (AValue (wrapValue (ANumber (AInteger 5)))),
              wrapExpr (AValue (wrapValue (ANumber (AFloat 3.5))))
            ]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testCastCharToInt :: Test
testCastCharToInt =
  TestCase
    ( assertBool
        "should compile cast from char to int"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACast (wrapType TInt) 
            (wrapExpr (AValue (wrapValue (ANumber (AChar 'A')))))
          )))) of
            Right [Push (VNumber (VChar 'A')), Cast NTInt] -> True
            _ -> False
        )
    )

testCastBoolToInt :: Test
testCastBoolToInt =
  TestCase
    ( assertBool
        "should compile cast from bool to int"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACast (wrapType TInt) 
            (wrapExpr (AValue (wrapValue (ANumber (ABool True)))))
          )))) of
            Right [Push (VNumber (VBool True)), Cast NTInt] -> True
            _ -> False
        )
    )

testModuloOperation :: Test
testModuloOperation =
  TestCase
    ( assertBool
        "should compile modulo operation"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall "%"))))
            [
              wrapExpr (AValue (wrapValue (ANumber (AInteger 10)))),
              wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
            ]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testGreaterOrEqualOperation :: Test
testGreaterOrEqualOperation =
  TestCase
    ( assertBool
        "should compile >= operation"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall ">="))))
            [
              wrapExpr (AValue (wrapValue (ANumber (AInteger 5)))),
              wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
            ]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testLogicalAnd :: Test
testLogicalAnd =
  TestCase
    ( assertBool
        "should compile logical AND operation"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall "&&"))))
            [
              wrapExpr (AValue (wrapValue (ANumber (ABool True)))),
              wrapExpr (AValue (wrapValue (ANumber (ABool False))))
            ]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testLogicalOr :: Test
testLogicalOr =
  TestCase
    ( assertBool
        "should compile logical OR operation"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
            (wrapExpr (AValue (wrapValue (AVarCall "||"))))
            [
              wrapExpr (AValue (wrapValue (ANumber (ABool True)))),
              wrapExpr (AValue (wrapValue (ANumber (ABool False))))
            ]
          )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testNestedStruct :: Test
testNestedStruct =
  TestCase
    ( assertBool
        "should compile nested struct"
        ( case compileWithEnv 
            (emptyEnv { 
              structDefs = M.fromList [
                ("Address", [(wrapType TString, "city"), (wrapType TInt, "zip")]),
                ("Person", [(wrapType TString, "name"), (wrapType (TStruct "Address"), "address")])
              ]
            })
            (wrapAst (AVarDecl (wrapType (TStruct "Person")) "p"
              (Just (wrapExpr (AValue (wrapValue (AStruct [
                ("name", wrapExpr (AValue (wrapValue (AString "Alice")))),
                ("address", wrapExpr (AValue (wrapValue (AStruct [
                  ("city", wrapExpr (AValue (wrapValue (AString "Paris")))),
                  ("zip", wrapExpr (AValue (wrapValue (ANumber (AInteger 75001)))))
                ]))))
              ]))))))) of
            Right _ -> True
            Left _ -> False
        )
    )

testArrayOfStructs :: Test
testArrayOfStructs =
  TestCase
    ( assertBool
        "should compile array of structs"
        ( case compileWithEnv 
            (emptyEnv { 
              structDefs = M.fromList [("Point", [(wrapType TInt, "x"), (wrapType TInt, "y")])]
            })
            (wrapAst (AVarDecl 
              (wrapType (TArray (wrapType (TStruct "Point")) (wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))))))
              "points"
              (Just (wrapExpr (AValue (wrapValue (AArray [
                wrapExpr (AValue (wrapValue (AStruct [
                  ("x", wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))),
                  ("y", wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))))
                ]))),
                wrapExpr (AValue (wrapValue (AStruct [
                  ("x", wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))),
                  ("y", wrapExpr (AValue (wrapValue (ANumber (AInteger 4)))))
                ])))
              ]))))))) of
            Right _ -> True
            Left _ -> False
        )
    )


kongCompilerTests :: [Test]
kongCompilerTests =
  [
    TestLabel "compile integer" testCompileInteger,
    TestLabel "compile float" testCompileFloat,
    TestLabel "compile boolean" testCompileBool,
    TestLabel "compile string" testCompileString,
    TestLabel "compile array" testCompileArray,
    TestLabel "compile vector" testCompileVector,
    TestLabel "compile tuple" testCompileTuple,
    TestLabel "compile struct" testCompileStruct,
    TestLabel "struct with heap string" testStructWithHeapString,
    TestLabel "array declaration and access" testArrayDeclarationAndAccess,
    TestLabel "array reassignment" testArrayReassignment,
    TestLabel "struct declaration and access" testStructDeclarationAndAccess,
    TestLabel "struct reassignment" testStructReassignment,
    TestLabel "vector declaration and access" testVectorDeclarationAndAccess,
    TestLabel "tuple declaration and access" testTupleDeclarationAndAccess,
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
    TestLabel "compile builtin mul" testCompileBuiltinMul,
    TestLabel "compile builtin div" testCompileBuiltinDiv,
    TestLabel "compile builtin less or equal" testCompileBuiltinLessOrEqual,
    TestLabel "compile builtin less than" testCompileBuiltinLessThan,
    TestLabel "compile builtin greater than" testCompileBuiltinGreaterThan,
    TestLabel "compile builtin not equal" testCompileBuiltinNotEqual,
    TestLabel "empty array" testEmptyArray,
    TestLabel "empty struct" testEmptyStruct,
    TestLabel "nested arrays" testNestedArrays,
    TestLabel "multiple var declarations" testMultipleVarDeclarations,
    TestLabel "konst variable" testKonstVariable,
    TestLabel "struct field access" testStructFieldAccess,
    TestLabel "out of bounds array access compiles" testOutOfBoundsArrayAccessCompiles,
    TestLabel "mixed type comparison" testMixedTypeComparison,
    TestLabel "cast int to float" testCastIntToFloat,
    TestLabel "cast float to int" testCastFloatToInt,
    TestLabel "empty block" testEmptyBlock,
    TestLabel "return with expression" testReturnWithExpression,
    TestLabel "struct definition" testStructDefinition,
    TestLabel "print string" testPrintString,
    TestLabel "print multiple args" testPrintMultipleArgs,
    TestLabel "string concat" testStringConcat,
    TestLabel "If/Else/Then" testIfElseCompile,
    TestLabel "Complex arithmetic" testComplexArithmetic,
    TestLabel "Function call" testNestedFunctionCalls,
    TestLabel "Int float operation" testMixedIntFloat,
    TestLabel "Cast char to int" testCastCharToInt,
    TestLabel "Cast bool to int" testCastBoolToInt,
    TestLabel "modulo operation" testModuloOperation,
    TestLabel ">= operation" testGreaterOrEqualOperation,
    TestLabel "And op" testLogicalAnd,
    TestLabel "or op" testLogicalOr,
    TestLabel "create Struct" testNestedStruct,
    TestLabel "Array of Struct" testArrayOfStructs
  ]
