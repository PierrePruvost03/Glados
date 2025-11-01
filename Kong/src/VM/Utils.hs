module VM.Utils (createList, createStruct, makeIntValue, makeBoolValue, mergeHeaps) where

import VM.Errors (ExecError (..))
import DataStruct.VM (Stack, Heap)
import DataStruct.Bytecode.Value (Value(..))
import DataStruct.Bytecode.Number (Number(..))
import Control.Exception
import Data.Char
import qualified Data.Vector as V

createList :: Stack -> Int -> ([Value], Stack)
createList stack n = f ([], stack) n
    where
        f r 0 = r
        f (_, []) _ = throw $ InvalidStackAccess
        f (l, (x:xs)) it = f (l <> [x], xs) (it - 1)

createStruct :: Stack -> [String] -> ([(String , Value)], Stack)
createStruct stack names = f ([], stack) names
    where
        f r [] = r
        f (_, []) _ = throw $ InvalidStackAccess
        f (l, vX:vXs) (nX:nXs) = f (l <> [(nX, vX)], vXs) nXs

makeBoolValue :: Value -> Bool
makeBoolValue (VNumber (VBool value)) = value
makeBoolValue (VNumber (VInt n))
    | n > 0 = True
    | otherwise = False
makeBoolValue (VNumber (VChar '\0')) = False
makeBoolValue (VNumber (VChar _)) = True
makeBoolValue (VNumber (VFloat n))
    | n > 0 = True
    | otherwise = False
makeBoolValue (VList (list)) = null list
-- makeBoolValue VStruct String (M.Map String HeapAddr)
-- makeBoolValue VFunction [String] [Instr] Env
-- makeBoolValue VRef HeapAddr
makeBoolValue VEmpty = False
makeBoolValue _ = False

makeIntValue :: Value -> Int
makeIntValue (VNumber (VInt i)) = fromIntegral i
makeIntValue (VNumber (VChar i)) = digitToInt i
makeIntValue (VNumber (VBool i)) = fromEnum i
makeIntValue _ = throw $ InvalidIntConversion

mergeHeaps :: Heap -> Heap -> Heap
mergeHeaps base retured = V.force (V.slice 0 (length base) retured)
