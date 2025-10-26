module Compiler.TypeError (
    TypeError(..),
    prettyTypeError
) where

 data TypeError
    = TypeMismatch { expected :: String, actual :: String }
    | InvalidComparison { leftType :: String, rightType :: String }
    | InvalidFunctionCall { functionName :: String, expectedType :: String, actualType :: String }
    | OtherTypeError String
    deriving (Eq, Show)

prettyTypeError :: TypeError -> String
prettyTypeError (TypeMismatch expected actual) = "Type mismatch: expected '" ++ expected ++ "', got '" ++ actual ++ "'"
prettyTypeError (InvalidComparison l r) = "Invalid comparison between '" ++ l ++ "' and '" ++ r ++ "'"
prettyTypeError (InvalidFunctionCall fname expT actT) = "Invalid call to function '" ++ fname ++ "': expected argument of type '" ++ expT ++ "', got '" ++ actT ++ "'"
prettyTypeError (OtherTypeError msg) = "Type error: " ++ msg
