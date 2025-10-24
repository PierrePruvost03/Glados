module KongCompilerTests (kongCompilerTests) where

import Data.List (isInfixOf)
import Test.HUnit

import Compiler.Program (compile)
import DataStruct.Ast
import DataStruct.Bytecode.Value (Instr(..), Value(..))
import DataStruct.Bytecode.Number (Number(..))
import DataStruct.Bytecode.Op (Op(..))
import Compiler.Types (CompilerError(..))
import qualified Data.Vector as V

testCompileInteger :: Test
testCompileInteger =
  TestCase
    ( assertEqual
        "should compile integer value to Push VInt instruction"
        (Right [Push (VNumber (VInt 42))])
        (compile (AExpress (AValue (ANumber (AInteger 42)))))
    )

testCompileFloat :: Test
testCompileFloat =
  TestCase $ do
    let result = compile (AExpress (AValue (ANumber (AFloat 3.14))))
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
        (compile (AExpress (AValue (ANumber (ABool True)))))
    )

testCompileString :: Test
testCompileString =
  TestCase
    ( assertEqual
        "should compile string value to Push VString instruction"
        (Right [Push (VList (V.fromList [VNumber (VChar 'h'), VNumber (VChar 'e'), VNumber (VChar 'l'), VNumber (VChar 'l'), VNumber (VChar 'o')]) False)])
        (compile (AExpress (AValue (AString "hello"))))
    )

testCompileArray :: Test
testCompileArray =
  TestCase
    ( assertEqual
        "should compile array literal to CreateList"
        (Right [Push (VNumber (VInt 1)), Push (VNumber (VInt 2)), Push (VNumber (VInt 3)), CreateList 3])
        (compile (AExpress (AValue (AArray [AValue (ANumber (AInteger 1)), AValue (ANumber (AInteger 2)), AValue (ANumber (AInteger 3))]))))
    )

testCompileVector :: Test
testCompileVector =
  TestCase
    ( assertEqual
        "should compile vector literal to CreateList"
        (Right [Push (VNumber (VInt 5)), Push (VNumber (VInt 6)), CreateList 2])
        (compile (AExpress (AValue (AVector [AValue (ANumber (AInteger 5)), AValue (ANumber (AInteger 6))]))))
    )

testCompileTuple :: Test
testCompileTuple =
  TestCase
    ( assertEqual
        "should compile tuple literal to CreateList"
        (Right [Push (VNumber (VInt 7)), Push (VNumber (VInt 8)), CreateList 2])
        (compile (AExpress (AValue (ATuple [AValue (ANumber (AInteger 7)), AValue (ANumber (AInteger 8))]))))
    )

testCompileStruct :: Test
testCompileStruct =
  TestCase
    ( assertEqual
        "should compile struct literal to CreateStruct"
        (Right [Push (VNumber (VInt 42)), Push (VList (V.fromList [VNumber (VChar 'a')]) False), CreateStruct ["name", "age"]])
        (compile (AExpress (AValue (AStruct [ ("name", AValue (AString "a"))
                                           , ("age", AValue (ANumber (AInteger 42)))
                                           ]))))
    )

testStructWithHeapString :: Test
testStructWithHeapString =
  TestCase
  ( assertEqual
    "should heapify string then load it when used in struct"
    (Right
      [ Push (VList (V.fromList [VNumber (VChar 'p'), VNumber (VChar 'i'), VNumber (VChar 'p'), VNumber (VChar 'i')]) False)
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
    ( compile
      ( ABlock
        [ AVarDecl TString "x" (Just (AValue (AString "pipi")))
        , AVarDecl
          (TStruct "Person")
          "a"
          ( Just
            ( AValue
              ( AStruct
                [ ("name", AValue (AVarCall "x"))
                , ("age", AValue (ANumber (AInteger 42)))
                ]
              )
            )
          )
        ]
      )
    )
  )

testArrayDeclarationAndAccess :: Test
testArrayDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile array declaration and access"
    (Right
      [ Push (VNumber (VInt 1))
      , Push (VNumber (VInt 2))
      , Push (VNumber (VInt 3))
      , CreateList 3
      , Alloc
      , StoreRef
      , SetVar "arr"
      , PushEnv "arr"
      , LoadRef
      , Push (VNumber (VInt 1))
      , GetList
      ])
    ( compile
      ( ABlock
                [ AVarDecl (TArray TInt (AValue (ANumber (AInteger 3)))) "arr"
          (Just (AValue (AArray [ AValue (ANumber (AInteger 1))
                       , AValue (ANumber (AInteger 2))
                       , AValue (ANumber (AInteger 3))
                       ])))
        , AExpress (AAccess (AArrayAccess "arr" (AValue (ANumber (AInteger 1)))))
        ]
      )
    )
  )

testArrayReassignment :: Test
testArrayReassignment =
  TestCase
  ( assertEqual
    "should compile array reassignment"
    (Right
      [ Push (VNumber (VInt 1))
      , Push (VNumber (VInt 2))
      , Push (VNumber (VInt 3))
      , CreateList 3
      , Alloc
      , StoreRef
      , SetVar "arr"
      , Push (VNumber (VInt 4))
      , Push (VNumber (VInt 5))
      , Push (VNumber (VInt 6))
      , CreateList 3
      , PushEnv "arr"
      , StoreRef
      ])
    ( compile
      ( ABlock
                [ AVarDecl (TArray TInt (AValue (ANumber (AInteger 3)))) "arr"
          (Just (AValue (AArray [ AValue (ANumber (AInteger 1))
                       , AValue (ANumber (AInteger 2))
                       , AValue (ANumber (AInteger 3))
                       ])))
        , AExpress
          (AAttribution "arr"
            (AValue (AArray [ AValue (ANumber (AInteger 4))
                     , AValue (ANumber (AInteger 5))
                     , AValue (ANumber (AInteger 6))
                     ])))
        ]
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
      , Push (VList (V.fromList [VNumber (VChar 'A'), VNumber (VChar 'd'), VNumber (VChar 'a')]) False)
      , CreateStruct ["name", "age"]
      , Alloc
      , StoreRef
      , SetVar "person"
      , PushEnv "person"
      , LoadRef
      , GetStruct "name"
      ])
    ( compile
      ( ABlock
        [ AVarDecl (TStruct "Person") "person"
          (Just (AValue (AStruct [ ("name", AValue (AString "Ada"))
                      , ("age", AValue (ANumber (AInteger 30)))
                      ])))
        , AExpress (AAccess (AStructAccess "person" ["name"]))
        ]
      )
    )
  )

testStructReassignment :: Test
testStructReassignment =
  TestCase
  ( assertEqual
    "should compile struct reassignment"
    (Right
      [ Push (VNumber (VInt 30))
      , Push (VList (V.fromList [VNumber (VChar 'A'), VNumber (VChar 'd'), VNumber (VChar 'a')]) False)
      , CreateStruct ["name", "age"]
      , Alloc
      , StoreRef
      , SetVar "person"
      , Push (VNumber (VInt 31))
      , Push (VList (V.fromList [VNumber (VChar 'B'), VNumber (VChar 'o'), VNumber (VChar 'b')]) False)
      , CreateStruct ["name", "age"]
      , PushEnv "person"
      , StoreRef
      ])
    ( compile
      ( ABlock
        [ AVarDecl (TStruct "Person") "person"
          (Just (AValue (AStruct [ ("name", AValue (AString "Ada"))
                      , ("age", AValue (ANumber (AInteger 30)))
                      ])))
        , AExpress
          (AAttribution "person"
            (AValue (AStruct [ ("name", AValue (AString "Bob"))
                     , ("age", AValue (ANumber (AInteger 31)))
                     ])))
        ]
      )
    )
  )

testVectorDeclarationAndAccess :: Test
testVectorDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile vector declaration and access"
    (Right
      [ Push (VNumber (VInt 10))
      , Push (VNumber (VInt 20))
      , CreateList 2
      , Alloc
      , StoreRef
      , SetVar "vec"
      , PushEnv "vec"
      , LoadRef
      , Push (VNumber (VInt 0))
      , GetList
      ])
    ( compile
      ( ABlock
                [ AVarDecl (TVector TInt (AValue (ANumber (AInteger 2)))) "vec"
          (Just (AValue (AVector [ AValue (ANumber (AInteger 10))
                       , AValue (ANumber (AInteger 20))
                       ])))
        , AExpress (AAccess (AVectorAccess "vec" (AValue (ANumber (AInteger 0)))))
        ]
      )
    )
  )

testTupleDeclarationAndAccess :: Test
testTupleDeclarationAndAccess =
  TestCase
  ( assertEqual
    "should compile tuple declaration and access"
    (Right
      [ Push (VNumber (VInt 5))
      , Push (VNumber (VInt 9))
      , CreateList 2
      , Alloc
      , StoreRef
      , SetVar "pair"
      , PushEnv "pair"
      , LoadRef
      , Push (VNumber (VInt 0))
      , GetList
      ])
    ( compile
      ( ABlock
        [ AVarDecl (TTuple [TInt, TInt]) "pair"
          (Just (AValue (ATuple [ AValue (ANumber (AInteger 5))
                      , AValue (ANumber (AInteger 9))
                      ])))
        , AExpress (AAccess (ATupleAccess "pair" (AValue (ANumber (AInteger 0)))))
        ]
      )
    )
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
    "should compile mutable variable declaration to heap-backed storage"
    (Right [Push (VNumber (VInt 0)), Alloc, StoreRef, SetVar "x"])
    (compile (AVarDecl TInt "x" Nothing))
  )

testCompileVarDeclWithValue :: Test
testCompileVarDeclWithValue =
  TestCase
  ( assertEqual
    "should compile mutable variable declaration with value to heap"
    (Right [Push (VNumber (VInt 42)), Alloc, StoreRef, SetVar "x"])
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
    ( assertBool
        "should fail attribution for undeclared variable"
        ( case compile (AExpress (AAttribution "x" (AValue (ANumber (AInteger 10))))) of
            Left (IllegalAssignment msg) -> "undeclared variable" `isInfixOf` msg
            _ -> False
        )
    )

testCompileFunctionCall :: Test
testCompileFunctionCall =
  TestCase
    ( assertEqual
        "should compile function call with arguments in reverse order"
        (Right [Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), PushEnv "myFunc", Call])
        (compile (AExpress (ACall "myFunc" [AValue (ANumber (AInteger 1)), AValue (ANumber (AInteger 2))])))
    )

testCompileBuiltinAdd :: Test
testCompileBuiltinAdd =
  TestCase
    ( assertEqual
        "should compile builtin addition operation"
        (Right [Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), DoOp Add])
        (compile (AExpress (ACall "+" [AValue (ANumber (AInteger 1)), AValue (ANumber (AInteger 2))])))
    )

testCompileBuiltinSub :: Test
testCompileBuiltinSub =
  TestCase
    ( assertEqual
        "should compile builtin subtraction operation"
        (Right [Push (VNumber (VInt 3)), Push (VNumber (VInt 5)), DoOp Sub])
        (compile (AExpress (ACall "-" [AValue (ANumber (AInteger 5)), AValue (ANumber (AInteger 3))])))
    )

testCompileBuiltinEqual :: Test
testCompileBuiltinEqual =
  TestCase
    ( assertEqual
        "should compile builtin equality operation"
        (Right [Push (VNumber (VInt 2)), Push (VNumber (VInt 1)), DoOp Equal])
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
        (Right [Push (VNumber (VInt 42)), Ret])
        (compile (AReturn (AExpress (AValue (ANumber (AInteger 42))))))
    )

testCompileBlock :: Test
testCompileBlock =
  TestCase
    ( assertEqual
        "should compile block of statements"
        (Right [Push (VNumber (VInt 5)), Alloc, StoreRef, SetVar "x", PushEnv "x", LoadRef])
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
    TestLabel "unsupported ast" testUnsupportedAst
  ]
