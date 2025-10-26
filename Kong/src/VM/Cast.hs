module VM.Cast (castNumber) where

import DataStruct.Bytecode.Number (Number(..), NumberType(..))

intCast :: Number -> Int
intCast (VInt i) = i
intCast (VChar i) = fromEnum i
intCast (VBool i) = fromEnum i
intCast (VFloat i) = floor i

charCast :: Number -> Char
charCast (VInt i) = toEnum (i `mod` 0x11000)
charCast (VChar i) = i
charCast (VBool i) = toEnum $ fromEnum i
charCast (VFloat i) = toEnum $ (floor i `mod` 0x11000)

boolCast :: Number -> Bool
boolCast (VInt i) = i /= 0
boolCast (VChar i) = i /= '\NUL'
boolCast (VBool i) = i
boolCast (VFloat i) = ((floor i) :: Int) /= 0

floatCast :: Number -> Double
floatCast (VInt i) = fromIntegral i
floatCast (VChar i) = fromIntegral (fromEnum i)
floatCast (VBool i) = fromIntegral (fromEnum i)
floatCast (VFloat i) = i

castNumber :: Number -> NumberType -> Number
castNumber n NTInt = VInt $ intCast n
castNumber n NTChar = VChar $ charCast n
castNumber n NTBool = VBool $ boolCast n
castNumber n NTFloat = VFloat $ floatCast n
