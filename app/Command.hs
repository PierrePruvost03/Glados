{-# LANGUAGE LambdaCase #-}
module Command (printHelp) where


commandList :: [(String, String)]
commandList = [
        ("help", "display the command list"),
        ("load", "execute the content of a file and load his symbols")]

printCommandHelp :: (String, String) -> IO ()
printCommandHelp (n, d) = putStrLn (':':(n <> "\t" <> d))

printHelp :: IO ()
printHelp = f commandList
    where
        f [] = return ()
        f (x:xs) = printCommandHelp x >> f xs
