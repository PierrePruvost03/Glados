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
  | VString String
  | VTuple (V.Vector Value) Bool
  | VArray (V.Vector Value) Bool        -- Bool = isKonst
  | VVector (V.Vector Value) Bool
  | VStruct (M.Map String HeapAddr) Bool
  | VFunction [String] (V.Vector Instr)
  | VBuiltinOp Op
  | VRef HeapAddr
  | VEmpty
  deriving (Eq, Show)


instance Binary Value where
    -- writing
    put (VNumber v) = put (0 :: Word8) <> put v
    put (VString v) = put (1 :: Word8) <> putList v
    put (VTuple v k) = put (2 :: Word8) <> putList (V.toList v) <> put k
    put (VArray v k) = put (3 :: Word8) <> putList (V.toList v) <> put k
    put (VVector v k) = put (4 :: Word8) <> putList (V.toList v) <> put k
    put (VStruct v k) = put (5 :: Word8) <> put (M.size v) <> put v <> put k
    put (VFunction a i) = put (6 :: Word8) <> putManyMany a <> putList (V.toList i)
    put (VBuiltinOp v) = put (7 :: Word8) <> put v
    put (VRef v) = put (8 :: Word8) <> put v
    put (VEmpty) = put (9 :: Word8)
    -- reading
    get = (get :: Get Word8) >>= \case
        0 -> construct VNumber
        1 -> constructList VString
        2 -> VTuple <$> (V.fromList <$> getList (get :: Get Value)) <*> (get :: Get Bool)
        3 -> VArray <$> (V.fromList <$> getList (get :: Get Value)) <*> (get :: Get Bool)
        4 -> VVector <$> (V.fromList <$> getList (get :: Get Value)) <*> (get :: Get Bool)
        5 -> VStruct <$> (get :: Get (M.Map String HeapAddr)) <*> (get :: Get Bool)
        6 -> VFunction <$> getList (getList (get :: Get Char)) <*> (V.fromList <$> getList (get :: Get Instr))
        7 -> construct VBuiltinOp
        8 -> construct VRef
        9 -> return VEmpty
        _ -> fail "Unknow Value"

data Instr
    = Push Value
    | PushEnv String
    | Call
    | Ret
    | Nop

    -- Assignations
    | SetVar String
    | SetArray String Int -- name index
    | SetVector String Int
    | SetStruct String String -- name field
    | SetTuple String Int -- name index

    -- Accès
    | GetArray Int
    | ArrayGet         -- Pour l'accès aux tableaux génériques
    | GetVector Int
    | GetStruct String
    | GetTuple Int

    -- Sauts
    | Jump Int
    | JumpIfFalse Int
    | JumpIfTrue Int

    -- Opérations natives
    | DoOp Op

    -- Fonctions
    | PushLambda [String] [Instr]

    -- Heap
    | Alloc
    | LoadRef
    | StoreRef
    deriving (Eq, Show)

instance Binary Instr where
    -- reading
    put (Push v) = put (0 :: Word8) <> put v
    put (PushEnv v) = put (1 :: Word8) <> putList v
    put Call = put (2 :: Word8)
    put Ret = put (3 :: Word8)
    put Nop = put (4 :: Word8)
    put (SetVar v) = put (5 :: Word8) <> putList v
    put (SetArray s v) = put (6 :: Word8) <> putList s <> put v
    put (SetVector s v) = put (7 :: Word8) <> putList s <> put v
    put (SetStruct s v) = put (8 :: Word8) <> putList s <> putList v
    put (SetTuple s v) = put (9 :: Word8) <> putList s <> put v
    put (GetArray v) = put (10 :: Word8) <> put v
    put ArrayGet = put (11 :: Word8)
    put (GetVector v) = put (12 :: Word8) <> put v
    put (GetStruct v) = put (13 :: Word8) <> putList v
    put (GetTuple v) = put (14 :: Word8) <> put v
    put (Jump v) = put (15 :: Word8) <> put v
    put (JumpIfFalse v) = put (16 :: Word8) <> put v
    put (JumpIfTrue v) = put (17 :: Word8) <> put v
    put (DoOp v) = put (18 :: Word8) <> put v
    put (PushLambda a i) = put (19 :: Word8) <> putList a <> putList i
    put Alloc = put (20 :: Word8)
    put LoadRef = put (21 :: Word8)
    put StoreRef = put (22 :: Word8)

    get = (get :: Get Word8) >>= \case
        0 -> construct Push
        1 -> constructList PushEnv
        2 -> return Call
        3 -> return Ret
        4 -> return Nop
        5 -> constructList SetVar
        6 -> SetArray <$> getList (get :: Get Char) <*> (get :: Get Int)
        7 -> SetVector <$> getList (get :: Get Char) <*> (get :: Get Int)
        8 -> SetStruct <$> getList (get :: Get Char) <*> getList (get :: Get Char)
        9 -> SetTuple <$> getList (get :: Get Char) <*> (get :: Get Int)
        10 -> construct GetArray
        11 -> return ArrayGet
        12 -> construct GetVector
        13 -> constructList GetStruct
        14 -> construct GetTuple
        15 -> construct Jump
        16 -> construct JumpIfFalse
        17 -> construct JumpIfTrue
        18 -> construct DoOp
        19 -> PushLambda <$> getList (getList (get :: Get Char)) <*> getList (get :: Get Instr)
        20 -> return Alloc
        21 -> return LoadRef
        22 -> return StoreRef
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
