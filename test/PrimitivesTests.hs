module PrimitivesTests (primitivesTests) where

import DataStruct.Value (Value (..))
import Interpreter.Primitives.Primitives (primAdd, primDiv, primEq, primGt, primLt, primMod, primMul, primSub)
import Test.HUnit

testPrimAddSuccess :: Test
testPrimAddSuccess =
  TestCase
    ( assertEqual
        "primAdd 2 + 3 = 5"
        (Right (VInt 5))
        (primAdd [VInt 2, VInt 3])
    )

testPrimAddFail :: Test
testPrimAddFail =
  TestCase
    ( assertEqual
        "primAdd with wrong args should fail with correct message"
        (Left "primAdd: expected integers, got [VInt 2,VString \"ZIZI\"]")
        (primAdd [VInt 2, VString "ZIZI"])
    )

testPrimSubSuccess :: Test
testPrimSubSuccess =
  TestCase
    ( assertEqual
        "primSub 5 - 3 = 2"
        (Right (VInt 2))
        (primSub [VInt 5, VInt 3])
    )

testPrimSubFail :: Test
testPrimSubFail =
  TestCase
    ( assertEqual
        "primSub should handle bool and int values"
        (Right $ VInt 4)
        (primSub [VInt 5, VBool True])
    )

testPrimMulSuccess :: Test
testPrimMulSuccess =
  TestCase
    ( assertEqual
        "primMul 4 * 3 = 12"
        (Right (VInt 12))
        (primMul [VInt 4, VInt 3])
    )

testPrimMulFail :: Test
testPrimMulFail =
  TestCase
    ( assertEqual
        "primMul should handle bool and int values"
        (Right $ VInt 4)
        (primMul [VInt 4, VBool True])
    )

testPrimDivSuccess :: Test
testPrimDivSuccess =
  TestCase
    ( assertEqual
        "primDiv 10 / 2 = 5"
        (Right (VInt 5))
        (primDiv [VInt 10, VInt 2])
    )

testPrimDivFail :: Test
testPrimDivFail =
  TestCase
    ( assertEqual
        "primDiv should handle bool and int values"
        (Right $ VInt 10)
        (primDiv [VInt 10, VBool True])
    )

testPrimDivByZero :: Test
testPrimDivByZero =
  TestCase
    ( assertEqual
        "primDiv by zero should fail with correct message"
        (Left "primDiv: division by zero")
        (primDiv [VInt 10, VInt 0])
    )

testPrimEqSuccess :: Test
testPrimEqSuccess =
  TestCase
    ( assertEqual
        "primEq 3 == 3 = True"
        (Right (VBool True))
        (primEq [VInt 3, VInt 3])
    )

testPrimEqFail :: Test
testPrimEqFail =
  TestCase
    ( assertEqual
        "primEq with wrong args should fail with correct message"
        (Left "primEq: expected two values, got [VInt 3]")
        (primEq [VInt 3])
    )

testPrimLtSuccess :: Test
testPrimLtSuccess =
  TestCase
    ( assertEqual
        "primLt 2 < 3 = True"
        (Right (VBool True))
        (primLt [VInt 2, VInt 3])
    )

testPrimLtFail :: Test
testPrimLtFail =
  TestCase
    ( assertEqual
        "primLt 2 < False (0) = False"
        (Right (VBool False))
        (primLt [VInt 2, VBool True])
    )

testPrimGtSuccess :: Test
testPrimGtSuccess =
  TestCase
    ( assertEqual
        "primGt 5 > 3 = True"
        (Right (VBool True))
        (primGt [VInt 5, VInt 3])
    )

testPrimGtFail :: Test
testPrimGtFail =
  TestCase
    ( assertEqual
        "primGt 5 > True (1) = True"
        (Right (VBool True))
        (primGt [VInt 5, VBool True])
    )

testPrimModSuccess :: Test
testPrimModSuccess =
  TestCase
    ( assertEqual
        "primMod 10 mod 3 = 1"
        (Right (VInt 1))
        (primMod [VInt 10, VInt 3])
    )

testPrimModFail :: Test
testPrimModFail =
  TestCase
    ( assertEqual
        "primMod with wrong args should fail with correct message"
        (Left "primMod: expected two integers, got [VInt 10,VBool True]")
        (primMod [VInt 10, VBool True])
    )

testPrimModByZero :: Test
testPrimModByZero =
  TestCase
    ( assertEqual
        "primMod by zero should fail with correct message"
        (Left "primMod: modulo by zero")
        (primMod [VInt 10, VInt 0])
    )

primitivesTests :: [Test]
primitivesTests =
  [ TestLabel "primAdd" testPrimAddSuccess,
    TestLabel "primAddFail" testPrimAddFail,
    TestLabel "primSub" testPrimSubSuccess,
    TestLabel "primSubFail" testPrimSubFail,
    TestLabel "primMul" testPrimMulSuccess,
    TestLabel "primMulFail" testPrimMulFail,
    TestLabel "primDiv" testPrimDivSuccess,
    TestLabel "primDivFail" testPrimDivFail,
    TestLabel "primDivByZero" testPrimDivByZero,
    TestLabel "primEq" testPrimEqSuccess,
    TestLabel "primEqFail" testPrimEqFail,
    TestLabel "primLt" testPrimLtSuccess,
    TestLabel "primLtFail" testPrimLtFail,
    TestLabel "primGt" testPrimGtSuccess,
    TestLabel "primGtFail" testPrimGtFail,
    TestLabel "primMod" testPrimModSuccess,
    TestLabel "primModFail" testPrimModFail,
    TestLabel "primModByZero" testPrimModByZero
  ]
