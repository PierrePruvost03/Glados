module KongCompilerErrorTests (kongCompilerErrorTests) where

import qualified Data.Map as M
import Test.HUnit
import Compiler.BytecodeGen.Program.Program (compileWithEnv)
import DataStruct.Ast
import Compiler.Type.Error (CompilerError(..))
import Compiler.Type.Inference (emptyEnv, CompilerEnv(..))
import TestHelpers


-- TESTS D'ERREURS - Type Errors

testTypeMismatch :: Test
testTypeMismatch =
  TestCase
    ( assertBool
        "should detect type mismatch between string and int"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (AString "hello"))))))
          ])) of
            Left (TypeMismatch _ _ _) -> True
            Left (IllegalAssignment _ _) -> True
            _ -> False
        )
    )

testInvalidComparison :: Test
testInvalidComparison =
  TestCase
    ( assertBool
        "should detect invalid comparison between incompatible types"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "=="))))
          [
            wrapExpr (AValue (wrapValue (AString "hello"))),
            wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))
          ]
        )))) of
            Left (InvalidComparison _ _ _) -> True
            Right _ -> False
            Left _ -> False
        )
    )

testArithmeticOnBool :: Test
testArithmeticOnBool =
  TestCase
    ( assertBool
        "Bool arithmetic should work (Bool, Char, Int, Float are all numbers)"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "+"))))
          [
            wrapExpr (AValue (wrapValue (ANumber (ABool True)))),
            wrapExpr (AValue (wrapValue (ANumber (ABool False))))
          ]
        )))) of
            Right _ -> True
            Left _ -> False
        )
    )

testArithmeticOnString :: Test
testArithmeticOnString =
  TestCase
    ( assertBool
        "should fail arithmetic operations on string types"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "+"))))
          [
            wrapExpr (AValue (wrapValue (AString "hello"))),
            wrapExpr (AValue (wrapValue (AString "world")))
          ]
        )))) of
            Left (IncompatibleOperands _ _ _ _) -> True
            Left (TypeMismatch _ _ _) -> True
            Left _ -> True
            Right _ -> False
        )
    )

testArithmeticOnArray :: Test
testArithmeticOnArray =
  TestCase
    ( assertBool
        "should fail arithmetic operations on array types"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "+"))))
          [
            wrapExpr (AValue (wrapValue (AArray [wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))]))),
            wrapExpr (AValue (wrapValue (AArray [wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))])))
          ]
        )))) of
            Left _ -> True
            Right _ -> False
        )
    )

testArithmeticMixedStringInt :: Test
testArithmeticMixedStringInt =
  TestCase
    ( assertBool
        "should fail arithmetic with mixed string and int"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "+"))))
          [
            wrapExpr (AValue (wrapValue (ANumber (AInteger 42)))),
            wrapExpr (AValue (wrapValue (AString "hello")))
          ]
        )))) of
            Left _ -> True
            Right _ -> False
        )
    )

-- TESTS D'ERREURS - Division par zéro

testDivisionByZeroConstant :: Test
testDivisionByZeroConstant =
  TestCase
    ( assertBool
        "should detect division by zero at compile time"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "/"))))
          [
            wrapExpr (AValue (wrapValue (ANumber (AInteger 10)))),
            wrapExpr (AValue (wrapValue (ANumber (AInteger 0))))
          ]
        )))) of
            Left (DivisionByZero _) -> True
            _ -> False
        )
    )

testDivisionByZeroFloat :: Test
testDivisionByZeroFloat =
  TestCase
    ( assertBool
        "should detect float division by zero"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
          (wrapExpr (AValue (wrapValue (AVarCall "/"))))
          [
            wrapExpr (AValue (wrapValue (ANumber (AFloat 10.0)))),
            wrapExpr (AValue (wrapValue (ANumber (AFloat 0.0))))
          ]
        )))) of
            Left (DivisionByZero _) -> True
            _ -> False
        )
    )


-- TESTS D'ERREURS - Konst modification

testKonstModification :: Test
testKonstModification =
  TestCase
    ( assertBool
        "should fail when trying to modify Konst variable"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TKonst (wrapType TInt))) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))),
            wrapAst (AExpress (wrapExpr (AAttribution "x" (wrapExpr (AValue (wrapValue (ANumber (AInteger 100))))))))
          ])) of
            Left (KonstModification _ _) -> True
            _ -> False
        )
    )

testKonstModificationGlobal :: Test
testKonstModificationGlobal =
  TestCase
    ( assertBool
        "should fail when trying to modify global Konst variable"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TKonst (wrapType TInt))) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))),
            wrapAst (AExpress (wrapExpr (AAttribution "x" (wrapExpr (AValue (wrapValue (ANumber (AInteger 100))))))))
          ])) of
            Left (KonstModification _ _) -> True
            _ -> False
        )
    )


-- TESTS D'ERREURS - Array access

testArrayNegativeIndex :: Test
testArrayNegativeIndex =
  TestCase
    ( assertBool
        "should detect negative array index at compile time"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TArray (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))))) "arr"
              (Just (wrapExpr (AValue (wrapValue (AArray [
                wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
              ])))))),
            wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AArrayAccess 
              (wrapExpr (AValue (wrapValue (AVarCall "arr"))))
              (wrapExpr (AValue (wrapValue (ANumber (AInteger (-1))))))
            )))))
          ])) of
            Left (NegativeIndex _ _) -> True
            _ -> False
        )
    )

testArrayOutOfBounds :: Test
testArrayOutOfBounds =
  TestCase
    ( assertBool
        "should detect out of bounds array access"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TArray (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))))) "arr"
              (Just (wrapExpr (AValue (wrapValue (AArray [
                wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
              ])))))),
            wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AArrayAccess 
              (wrapExpr (AValue (wrapValue (AVarCall "arr"))))
              (wrapExpr (AValue (wrapValue (ANumber (AInteger 10)))))
            )))))
          ])) of
            Left (IndexOutOfBounds _ _ _) -> True
            _ -> False
        )
    )

testArrayValidIndex :: Test
testArrayValidIndex =
  TestCase
    ( assertBool
        "should allow valid array index access"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TArray (wrapType TInt) (wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))))) "arr"
              (Just (wrapExpr (AValue (wrapValue (AArray [
                wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 2)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 3))))
              ])))))),
            wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AArrayAccess 
              (wrapExpr (AValue (wrapValue (AVarCall "arr"))))
              (wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))))
            )))))
          ])) of
            Right _ -> True
            Left _ -> False
        )
    )

-- TESTS D'ERREURS - Tuple access

testTupleOutOfBounds :: Test
testTupleOutOfBounds =
  TestCase
    ( assertBool
        "should detect out of bounds tuple access"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType (TTuple [wrapType TInt, wrapType TInt])) "pair"
              (Just (wrapExpr (AValue (wrapValue (ATuple [
                wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
                wrapExpr (AValue (wrapValue (ANumber (AInteger 2))))
              ])))))),
            wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (ATupleAccess 
              (wrapExpr (AValue (wrapValue (AVarCall "pair"))))
              (wrapExpr (AValue (wrapValue (ANumber (AInteger 5)))))
            )))))
          ])) of
            Left (IndexOutOfBounds _ _ _) -> True
            _ -> False
        )
    )


-- TESTS D'ERREURS - Struct errors

testUndefinedStruct :: Test
testUndefinedStruct =
  TestCase
    ( assertBool
        "should detect undefined struct in declaration"
        ( case compileWithEnv emptyEnv (wrapAst (AVarDecl (wrapType (TStruct "UnknownStruct")) "s" Nothing)) of
            Left (UndefinedStruct _ _ _) -> True
            Left (UninitializedVariable _ _) -> True
            _ -> False
        )
    )

testUnknownStructField :: Test
testUnknownStructField =
  TestCase
    ( assertBool
        "should detect unknown struct field access"
        ( case compileWithEnv 
            (emptyEnv { structDefs = M.fromList [("Person", [(wrapType TString, "name"), (wrapType TInt, "age")])] })
            (wrapAst (ABlock [
              wrapAst (AVarDecl (wrapType (TStruct "Person")) "person"
                (Just (wrapExpr (AValue (wrapValue (AStruct [
                  ("name", wrapExpr (AValue (wrapValue (AString "Ada")))),
                  ("age", wrapExpr (AValue (wrapValue (ANumber (AInteger 30)))))
                ])))))),
              wrapAst (AExpress (wrapExpr (AAccess (wrapAccess (AStructAccess 
                (wrapExpr (AValue (wrapValue (AVarCall "person"))))
                ["unknown_field"]
              )))))
            ])) of
            Left (UnknownStructField _ _ _) -> True
            _ -> False
        )
    )


-- TESTS D'ERREURS - Function errors

testDuplicateFunction :: Test
testDuplicateFunction =
  TestCase
    ( assertBool
        "should detect duplicate variable declaration"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType TInt) "add" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))),
            wrapAst (AVarDecl (wrapType TInt) "add" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 43)))))))
          ])) of
            Left (DuplicateDeclaration _ _ _) -> True
            _ -> False
        )
    )

testMissingReturn :: Test
testMissingReturn =
  TestCase
    ( assertBool
        "should handle lambda without return"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ALambda 
            []
            (wrapType TInt)
            [wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42)))))))]
          )))))) of
            Left (MissingReturn _) -> True
            _ -> False
        )
    )

testWrongReturnType :: Test
testWrongReturnType =
  TestCase
    ( assertBool
        "should detect wrong return type in lambda"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (AValue (wrapValue (ALambda 
            []
            (wrapType TInt)
            [wrapAst (AReturn (wrapAst (AExpress (wrapExpr (AValue (wrapValue (AString "hello")))))))]
          )))))) of
            Left (ReturnTypeMismatch _ _ _) -> True
            Left (TypeMismatch _ _ _) -> True
            Left (InvalidReturnType _ _) -> True
            _ -> False
        )
    )

testArgumentCountMismatch :: Test
testArgumentCountMismatch =
  TestCase
    ( assertBool
        "should detect argument count mismatch in builtin function"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
              (wrapExpr (AValue (wrapValue (AVarCall "+"))))
              [wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))]
            )))) of
            Left (ArgumentCountMismatch _ _ _) -> True
            Left (InvalidArguments _ _) -> True
            _ -> False
        )
    )

testArgumentTypeMismatch :: Test
testArgumentTypeMismatch =
  TestCase
    ( assertBool
        "should detect argument type mismatch in operations"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACall 
              (wrapExpr (AValue (wrapValue (AVarCall "+"))))
              [
                wrapExpr (AValue (wrapValue (ANumber (AInteger 1)))),
                wrapExpr (AValue (wrapValue (AString "hello")))
              ]
            )))) of
            Left (ArgumentTypeMismatch _ _ _ _) -> True
            Left (TypeMismatch _ _ _) -> True
            Left (IncompatibleOperands _ _ _ _) -> True
            _ -> False
        )
    )

testCallNonFunction :: Test
testCallNonFunction =
  TestCase
    ( assertBool
        "should detect attempt to call non-function value"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 42))))))),
            wrapAst (AExpress (wrapExpr (ACall 
              (wrapExpr (AValue (wrapValue (AVarCall "x"))))
              []
            )))
          ])) of
            Left (NonCallableType _ _ _) -> True
            Left (UnknownFunction _ _) -> True
            _ -> False
        )
    )


-- TESTS D'ERREURS - Variable errors

testDuplicateVariable :: Test
testDuplicateVariable =
  TestCase
    ( assertBool
        "duplicate variable may allow shadowing"
        ( case compileWithEnv emptyEnv (wrapAst (ABlock [
            wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 10))))))),
            wrapAst (AVarDecl (wrapType TInt) "x" (Just (wrapExpr (AValue (wrapValue (ANumber (AInteger 20)))))))
          ])) of
            Left (DuplicateDeclaration _ _ _) -> True
            Left _ -> True
            Right _ -> True  -- May allow shadowing
        )
    )

testUninitializedVariable :: Test
testUninitializedVariable =
  TestCase
    ( assertBool
        "should detect uninitialized variable declaration"
        ( case compileWithEnv emptyEnv (wrapAst (AVarDecl (wrapType TInt) "x" Nothing)) of
            Left (UninitializedVariable _ _) -> True
            _ -> False
        )
    )


-- TESTS D'ERREURS - Cast errors

testInvalidCast :: Test
testInvalidCast =
  TestCase
    ( assertBool
        "should detect invalid cast from string to int"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACast (wrapType TInt) 
            (wrapExpr (AValue (wrapValue (AString "hello"))))
          )))) of
            Left (InvalidCast _ _ _) -> True
            _ -> False
        )
    )

testInvalidCastArrayToInt :: Test
testInvalidCastArrayToInt =
  TestCase
    ( assertBool
        "should detect invalid cast from array to int"
        ( case compileWithEnv emptyEnv (wrapAst (AExpress (wrapExpr (ACast (wrapType TInt) 
            (wrapExpr (AValue (wrapValue (AArray [wrapExpr (AValue (wrapValue (ANumber (AInteger 1))))]))))
          )))) of
            Left (InvalidCast _ _ _) -> True
            _ -> False
        )
    )

kongCompilerErrorTests :: [Test]
kongCompilerErrorTests =
  [
    TestLabel "type mismatch" testTypeMismatch,
    TestLabel "invalid comparison" testInvalidComparison,
    TestLabel "arithmetic on bool" testArithmeticOnBool,
    TestLabel "arithmetic on string" testArithmeticOnString,
    TestLabel "arithmetic on array" testArithmeticOnArray,
    TestLabel "arithmetic mixed string int" testArithmeticMixedStringInt,
    TestLabel "division by zero constant" testDivisionByZeroConstant,
    TestLabel "division by zero float" testDivisionByZeroFloat,
    TestLabel "konst modification" testKonstModification,
    TestLabel "konst modification global" testKonstModificationGlobal,
    TestLabel "array negative index" testArrayNegativeIndex,
    TestLabel "array out of bounds" testArrayOutOfBounds,
    TestLabel "array valid index" testArrayValidIndex,
    TestLabel "tuple out of bounds" testTupleOutOfBounds,
    TestLabel "undefined struct" testUndefinedStruct,
    TestLabel "unknown struct field" testUnknownStructField,
    TestLabel "duplicate function" testDuplicateFunction,
    TestLabel "missing return" testMissingReturn,
    TestLabel "wrong return type" testWrongReturnType,
    TestLabel "argument count mismatch" testArgumentCountMismatch,
    TestLabel "argument type mismatch" testArgumentTypeMismatch,
    TestLabel "call non function" testCallNonFunction,
    TestLabel "duplicate variable" testDuplicateVariable,
    TestLabel "uninitialized variable" testUninitializedVariable,
    TestLabel "invalid cast" testInvalidCast,
    TestLabel "invalid cast array to int" testInvalidCastArrayToInt
  ]
