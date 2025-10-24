{-# LANGUAGE LambdaCase #-}
module DataStruct.Bytecode.Value (Value(..), Instr(..), put, get) where

import qualified Data.Vector as V
import qualified Data.Map as M
import DataStruct.Bytecode.Number (Number(..))
import Data.Binary
import DataStruct.Bytecode.Utils (putManyMany, construct, constructList, getList)
import DataStruct.Bytecode.Op (Op(..))

type Env = M.Map String Value
type HeapAddr = Int

data Value
  = VNumber Number
  | VList (V.Vector Value) Bool
  | VStruct (M.Map String Value) Bool
  | VFunction [String] (V.Vector Instr)
  | VBuiltinOp Op
  | VRef HeapAddr
  | VEmpty
  deriving (Eq, Show)


instance Binary Value where
    -- writing
    put (VNumber v) = put (0 :: Word8) <> put v
    put (VList v k) = put (2 :: Word8) <> putList (V.toList v) <> put k
    put (VStruct v k) = put (3 :: Word8) <> put v <> put k
    put (VFunction a i) = put (4 :: Word8) <> putManyMany a <> putList (V.toList i)
    put (VBuiltinOp v) = put (5 :: Word8) <> put v
    put (VRef v) = put (6 :: Word8) <> put v
    put (VEmpty) = put (7 :: Word8)
    -- reading
    get = (get :: Get Word8) >>= \case
        0 -> construct VNumber
        2 -> VList <$> (V.fromList <$> getList (get :: Get Value)) <*> (get :: Get Bool)
        3 -> VStruct <$> (get :: Get (M.Map String Value)) <*> (get :: Get Bool)
        4 -> VFunction <$> getList (getList (get :: Get Char)) <*> (V.fromList <$> getList (get :: Get Instr))
        5 -> construct VBuiltinOp
        6 -> construct VRef
        7 -> return VEmpty
        _ -> fail "Unknow Value"

data Instr
    = Push Value
    | PushEnv String
    | Call
    | Ret
    | Nop

    -- Assignations
    | SetVar String     -- stack state (value : xs)
    | SetList           -- stack state (list : index : value : xs)
    | SetStruct String  -- stack state (struct : value : xs)

    -- Accès
    | GetList           -- stack state (list : index : xs)
    | GetStruct String  -- stack state (struct : xs) -- field pas dans la stack car impossible d'acceder à un field avec une expression

    -- Creation
    | CreateList Int        -- stack access for int = 2 (value : value : xs)
    | CreateStruct [String] -- stack access for array = ["age", "name"] (age value : name value : xs)

    -- Sauts
    | Jump Int
    | JumpIfFalse Int
    | JumpIfTrue Int

    -- Opérations natives
    | DoOp Op

    -- Heap
    | Alloc
    | LoadRef
    | StoreRef              -- stack state (addr : value : xs)
    deriving (Eq, Show)

instance Binary Instr where
    -- reading
    put (Push v) = put (0 :: Word8) <> put v
    put (PushEnv v) = put (1 :: Word8) <> putList v
    put Call = put (2 :: Word8)
    put Ret = put (3 :: Word8)
    put Nop = put (4 :: Word8)
    put (SetVar v) = put (5 :: Word8) <> putList v
    put (SetList) = put (6 :: Word8)
    put (SetStruct s) = put (7 :: Word8) <> putList s
    put (GetList) = put (8 :: Word8)
    put (GetStruct v) = put (9 :: Word8) <> putList v
    put (CreateList i) = put (10 :: Word8) <> put i
    put (CreateStruct l) = put (11 :: Word8) <> putManyMany l
    put (Jump v) = put (12 :: Word8) <> put v
    put (JumpIfFalse v) = put (13 :: Word8) <> put v
    put (JumpIfTrue v) = put (14 :: Word8) <> put v
    put (DoOp v) = put (15 :: Word8) <> put v
    put Alloc = put (16 :: Word8)
    put LoadRef = put (17 :: Word8)
    put StoreRef = put (18 :: Word8)

    get = (get :: Get Word8) >>= \case
        0 -> construct Push
        1 -> constructList PushEnv
        2 -> return Call
        3 -> return Ret
        4 -> return Nop
        5 -> constructList SetVar
        6 -> return SetList
        7 -> constructList SetStruct
        8 -> return GetList
        9 -> constructList GetStruct
        10 -> construct CreateList
        11 -> CreateStruct <$> getList (getList (get :: Get Char))
        12 -> construct Jump
        13 -> construct JumpIfFalse
        14 -> construct JumpIfTrue
        15 -> construct DoOp
        16 -> return Alloc
        17 -> return LoadRef
        18 -> return StoreRef
        _ -> fail "Unknow Insrtuction"




-- Int p = 5;

-- Funk c(Int x) -> Int {
--     Int d = 5; //  p = 5, c [] , x
--     Int d = 3;
--     p = 3;
--     Return d; // p = 5, c [] d = 5
-- }

-- PushEnv x

-- Funk a(Int x) -> Int {
--     // pushEnv x
--     Int c = 4; // p = 5, x, a [],c []

--     Funk b(Int y) -> Int {
--         Return y + c; //
--     }
--     p = p + 4
--     c = c + 1;

--     Int d = 5;
--     Return b(4);
-- }


-- Push "Main"
-- Call
