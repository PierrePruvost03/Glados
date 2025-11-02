{-# LANGUAGE OverloadedLists #-}
module KongVMTests (kongVMTests) where

import qualified Data.Vector as V
import qualified Data.Map as M
import Test.HUnit

import DataStruct.Bytecode.Value
import DataStruct.Bytecode.Number
import DataStruct.VM
import VM.Execution (exec)

testPush :: Test
testPush = TestCase $ do
  result <- exec (baseState [Push (VNumber (VInt 42)), Ret] [])
  assertEqual
    "should push 42 on stack"
    (VMState
      { stack = [VNumber (VInt 42)]
      , heap = V.empty
      , env = M.fromList (baseEnv)
      , ip = 1
      , code = V.fromList [Push (VNumber (VInt 42)), Ret]
      , args = VList $ V.fromList []
      })
    result

testPushEnvAndSetVar :: Test
testPushEnvAndSetVar = TestCase $ do
  result <- exec (baseState [Push (VNumber (VInt 10)), SetVar "x", PushEnv "x", Ret] [])
  assertEqual
    "should set and then push variable from env"
    (VMState
      { stack = [VNumber (VInt 10)]
      , heap = V.empty
      , env = M.fromList (baseEnv <> [("x", VNumber (VInt 10))])
      , ip = 3
      , code = V.fromList [Push (VNumber (VInt 10)), SetVar "x", PushEnv "x", Ret]
      , args = VList $ V.fromList []
      })
    result

testCall :: Test
testCall = TestCase $ do
  let funcBody = [Push (VNumber (VInt 7)), Ret]
      func = VFunction [] (V.fromList funcBody)
  result <- exec (baseState [Push func, Call, Ret] [])
  assertEqual
    "should call function and push 7"
    (VMState
      { stack = [VNumber (VInt 7)]
      , heap = V.empty
      , env = M.fromList (baseEnv)
      , ip = 2
      , code = V.fromList [Push func, Call, Ret]
      , args = VList $ V.fromList []
      })
    result

testNop :: Test
testNop = TestCase $ do
  result <- exec (baseState [Nop, Ret] [])
  assertEqual
    "should skip Nop and end with empty stack"
    (VMState
      { stack = []
      , heap = V.empty
      , env = M.fromList (baseEnv)
      , ip = 1
      , code = V.fromList [Nop, Ret]
      , args = VList $ V.fromList []
      })
    result

testCreateList :: Test
testCreateList = TestCase $ do
  result <- exec (baseState [Push (VNumber (VInt 1)), Push (VNumber (VInt 2)), CreateList 2, Ret] [])
  assertEqual
    "should create list from top 2 values"
    (VMState
      { stack = [VList (V.fromList [VNumber (VInt 2), VNumber (VInt 1)])]
      , heap = V.empty
      , env = M.fromList (baseEnv)
      , ip = 3
      , code = V.fromList [Push (VNumber (VInt 1)), Push (VNumber (VInt 2)), CreateList 2, Ret]
      , args = VList $ V.fromList []
      })
    result

testCreateStruct :: Test
testCreateStruct = TestCase $ do
  result <- exec (baseState
    [ Push (VNumber (VInt 25))
    , Push (VNumber (VInt 99))
    , CreateStruct ["age", "score"]
    , Ret
    ] [])
  let expectedStruct = M.fromList [("age", VNumber (VInt 99)), ("score", VNumber (VInt 25))]
  assertEqual
    "should create struct"
    (VMState
      { stack = [VStruct expectedStruct]
      , heap = V.empty
      , env = M.fromList (baseEnv)
      , ip = 3
      , code = V.fromList
          [ Push (VNumber (VInt 25))
          , Push (VNumber (VInt 99))
          , CreateStruct ["age", "score"]
          , Ret
          ]
      , args = VList $ V.fromList []
      })
    result

testJump :: Test
testJump = TestCase $ do
  result <- exec (baseState [Jump 1, Push (VNumber (VInt 5)), Ret] [])
  assertEqual
    "should jump to instruction index 1"
    (VMState
      { stack = [VNumber (VInt 5)]
      , heap = V.empty
      , env = M.fromList (baseEnv)
      , ip = 2
      , code = V.fromList [Jump 1, Push (VNumber (VInt 5)), Ret]
      , args = VList $ V.fromList []
      })
    result

testHeapOps :: Test
testHeapOps = TestCase $ do
  result <- exec (baseState [Push (VNumber (VInt 9)), Alloc, StoreRef, LoadRef, Ret] [])
  assertEqual
    "should store value in heap and reload it"
    (VMState
      { stack = [VNumber (VInt 9)]
      , heap = V.fromList [VNumber (VInt 9)]
      , env = M.fromList (baseEnv)
      , ip = 4
      , code = V.fromList [Push (VNumber (VInt 9)), Alloc, StoreRef, LoadRef, Ret]
      , args = VList $ V.fromList []
      })
    result

testFunctionWithEnv :: Test
testFunctionWithEnv = TestCase $ do
    let funcBody =
            [ Alloc
            , StoreRef
            , SetVar "a"
            , Alloc
            , StoreRef
            , SetVar "b"
            , PushEnv "b"
            , LoadRef
            , PushEnv "a"
            , LoadRef
            , CreateList 2
            , Ret
            ]
        func = VFunction [] (V.fromList funcBody)
    result <- exec (baseState
        [ Push func
        , SetVar "makePair"
        , Push (VNumber (VInt 1))
        , Push (VNumber (VInt 2))
        , PushEnv "makePair"
        , Call
        , Ret
        ] [])
    assertEqual
        "should create [2,1] from function call"
        (VMState
            { stack = [VList (V.fromList [VNumber (VInt 2), VNumber (VInt 1)])]
            , heap = V.fromList [VNumber (VInt 2), VNumber (VInt 1)]
            , env = M.fromList (baseEnv <> [("makePair", func)])
            , ip = 6
            , code = V.fromList
                [ Push func
                , SetVar "makePair"
                , Push (VNumber (VInt 1))
                , Push (VNumber (VInt 2))
                , PushEnv "makePair"
                , Call
                , Ret
                ]
            , args = VList $ V.fromList []
            })
        result

testNestedStructAccess :: Test
testNestedStructAccess = TestCase $ do
    let inner = VStruct (M.fromList [("x", VNumber (VInt 10))])
        outerFields = M.fromList [("inner", inner), ("label", VNumber (VInt 1))]
    result <- exec (baseState
        [ Push (VStruct outerFields)
        , SetVar "outer"
        , PushEnv "outer"
        , GetStruct "inner"
        , GetStruct "x"
        , Ret
        ] [])
    assertEqual
        "should access nested struct field outer.inner.x == 10"
        (VMState
            { stack = [VNumber (VInt 10)]
            , heap = V.empty
            , env = M.fromList (baseEnv <> [("outer", VStruct outerFields)])
            , ip = 5
            , code = V.fromList
                [ Push (VStruct outerFields)
                , SetVar "outer"
                , PushEnv "outer"
                , GetStruct "inner"
                , GetStruct "x"
                , Ret
                ]
            , args = VList $ V.fromList []
            })
        result

testListMutation :: Test
testListMutation = TestCase $ do
    let initList = VList (V.fromList [VNumber (VInt 0), VNumber (VInt 0)])
    result <- exec (baseState
        [ Push initList
        , SetVar "arr"
        , Push (VNumber (VInt 99)) -- value
        , Push (VNumber (VInt 1))  -- index
        , PushEnv "arr"
        , SetList
        , SetVar "arr"
        , Push (VNumber (VInt 1))
        , PushEnv "arr"
        , GetList
        , Ret
        ] [])
    assertEqual
        "should update arr[1] = 99 and read back 99"
        (VMState
            { stack = [VNumber (VInt 99)]
            , heap = V.empty
            , env = M.fromList (baseEnv <> [("arr", VList (V.fromList [VNumber (VInt 0), VNumber (VInt 99)]))])
            , ip = 10
            , code = V.fromList
                [ Push initList
                , SetVar "arr"
                , Push (VNumber (VInt 99)) -- value
                , Push (VNumber (VInt 1))  -- index
                , PushEnv "arr"
                , SetList
                , SetVar "arr"
                , Push (VNumber (VInt 1))
                , PushEnv "arr"
                , GetList
                , Ret
                ]
            , args = VList $ V.fromList []
            })
        result

testHeapStructIntegration :: Test
testHeapStructIntegration = TestCase $ do
    let personFields = ["age","id"]
    result <- exec (baseState
        [ Push (VNumber (VInt 21))
        , Push (VNumber (VInt 123))
        , CreateStruct personFields
        , Alloc
        , StoreRef
        , LoadRef
        , SetVar "person"
        , PushEnv "person"
        , GetStruct "age"
        , Ret
        ] [])
    let expectedStruct = VStruct (M.fromList ([("age", VNumber (VInt 123)), ("id", VNumber (VInt 21))]))
    assertEqual
        "should create struct in heap and access field"
        (VMState
            { stack = [VNumber (VInt 123)]
            , heap = V.fromList [expectedStruct]
            , env = M.fromList (baseEnv <> [("person", expectedStruct)])
            , ip = 9
            , code = V.fromList
                [ Push (VNumber (VInt 21))
                , Push (VNumber (VInt 123))
                , CreateStruct personFields
                , Alloc
                , StoreRef
                , LoadRef
                , SetVar "person"
                , PushEnv "person"
                , GetStruct "age"
                , Ret
                ]
            , args = VList $ V.fromList []
            })
        result

kongVMTests :: [Test]
kongVMTests =
    [ TestLabel "Push" testPush
    , TestLabel "PushEnv and SetVar" testPushEnvAndSetVar
    , TestLabel "Call" testCall
    , TestLabel "Nop" testNop
    , TestLabel "CreateList" testCreateList
    , TestLabel "CreateStruct" testCreateStruct
    , TestLabel "Jump" testJump
    , TestLabel "Heap Ops" testHeapOps
    , TestLabel "Function with Env" testFunctionWithEnv
    , TestLabel "Nested Struct Access" testNestedStructAccess
    , TestLabel "List Mutation" testListMutation
    , TestLabel "Heap/Struct Integration" testHeapStructIntegration
    ]
