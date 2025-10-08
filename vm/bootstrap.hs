
data Value = Num Int | Boolean Bool deriving (Eq, Ord, Show)

data Operation = Add | Sub | Mul | Div | Eq | Less

data Instruction = Push Value | Call Operation | Ret | JumpIfFalse Int | PushArg Int

type Stack = [Value]

type Insts = [Instruction]

type Args = [Value]

calculate :: Operation -> Value -> Value -> Value
calculate c (Boolean b) v
    | b = calculate c (Num 1) v
    | otherwise = calculate c (Num 0) v
calculate c v (Boolean b)
    | b = calculate c (Num 1) v
    | otherwise = calculate c (Num 0) v
calculate Add (Num n1) (Num n2) = Num $ n1 + n2
calculate Sub (Num n1) (Num n2) = Num $ n1 - n2
calculate Mul (Num n1) (Num n2) = Num $ n1 * n2
calculate Div (Num n1) (Num n2) = Num $ n1 `div` n2
calculate Eq (Num n1) (Num n2) = Boolean $ n1 == n2
calculate Less (Num n1) (Num n2) = Boolean $ n1 < n2

exec :: Args -> Insts -> Stack -> Either String Value
exec _ [] _ = Left "Nothing left to do"
exec _ (Ret : _) [] = Left "No value to return"
exec _ (Ret : _) (x : _) = Right x
exec a (Push x : xs) st = exec a xs $ [x] <> st
exec _ (Call x : xs) (st : []) = Left "Not enough arguments"
exec _ (Call (Div) : xs) (_ : Num 0 : _) = Left "Division by zero"
exec _ (Call (Div) : xs) (_ : Boolean False : _) = Left "Division by zero"
exec a (Call x : xs) (st1 : st2 : sts) = exec a xs $ [calculate x st1 st2] <> sts
exec _ (JumpIfFalse _ : _) [] = Left "No value to check"
exec a (JumpIfFalse x : xs) (Boolean False : sts)
    | x < length xs = exec a (drop x xs) sts
    | otherwise = Left "Jumping out of range"
exec a (JumpIfFalse _ : xs) (st : sts) = exec a xs sts
exec a (PushArg x : xs) (sts)
    | x >= length a = Left "Argument out of range"
    | otherwise = exec a xs $ [xa] <> sts
    where xa = a !! x

