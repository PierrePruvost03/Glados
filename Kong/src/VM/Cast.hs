module VM.Cast (castNumber) where

import DataStruct.Bytecode.Number (Number(..), NumberType(..))
import Data.Word (Word32)
import Data.Int (Int32, Int64)

intCast :: Number -> Int32
intCast (VInt i) = i
intCast (VChar i) = fromIntegral (fromEnum i)
intCast (VBool i) = fromIntegral (fromEnum i)
intCast (VFloat i) = floor i
intCast (VLong i) = fromIntegral i
intCast (VUInt i) = fromIntegral i

charCast :: Number -> Char
charCast (VInt i) = toEnum (fromIntegral i `mod` 0x11000)
charCast (VChar i) = i
charCast (VBool i) = toEnum $ fromEnum i
charCast (VFloat i) = toEnum $ (floor i `mod` 0x11000)
charCast (VLong i) = toEnum $ (fromIntegral i `mod` 0x11000)
charCast (VUInt i) = toEnum $ (fromIntegral i `mod` 0x11000)

boolCast :: Number -> Bool
boolCast (VInt i) = i /= 0
boolCast (VChar i) = i /= '\NUL'
boolCast (VBool i) = i
boolCast (VFloat i) = ((floor i) :: Int) /= 0
boolCast (VLong i) = i /= 0
boolCast (VUInt i) = i /= 0

floatCast :: Number -> Double
floatCast (VInt i) = fromIntegral i
floatCast (VChar i) = fromIntegral (fromEnum i)
floatCast (VBool i) = fromIntegral (fromEnum i)
floatCast (VFloat i) = i
floatCast (VLong i) = fromIntegral i
floatCast (VUInt i) = fromIntegral i

longCast :: Number -> Int64
longCast (VInt i) = fromIntegral i
longCast (VChar i) = fromIntegral (fromEnum i)
longCast (VBool i) = fromIntegral (fromEnum i)
longCast (VFloat i) = floor i
longCast (VLong i) = i
longCast (VUInt i) = fromIntegral i

uintCast :: Number -> Word32
uintCast (VInt i) = fromIntegral (max 0 i)
uintCast (VChar i) = fromIntegral (fromEnum i)
uintCast (VBool i) = fromIntegral (fromEnum i)
uintCast (VFloat i) = fromIntegral (max 0 (floor i :: Int64))
uintCast (VLong i) = fromIntegral (max 0 i)
uintCast (VUInt i) = i

castNumber :: Number -> NumberType -> Number
castNumber n NTInt = VInt $ intCast n
castNumber n NTChar = VChar $ charCast n
castNumber n NTBool = VBool $ boolCast n
castNumber n NTFloat = VFloat $ floatCast n
castNumber n NTLong = VLong $ longCast n
castNumber n NTUInt = VUInt $ uintCast n
