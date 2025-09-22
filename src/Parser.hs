module Parser (
    Parser,
    runParser,
    parseChar,
    -- parseString,
    -- parseInt,
    -- parseFloat,
    -- parseCharAny,
    -- parseAnyChar,
    -- parseNotChar,
    -- checkEmptyString,
    -- skipChars,
    -- parseUntilChar,
    -- parseManyWithSeparator,
    -- parseNotCharAny,
    -- parseUntilAnyChar,
    -- fmap,
    -- pure,
    -- (<*>),
    -- empty,
    -- (<|>),
    -- (>>=),
    -- parseSomeUntilAnyChar,
    -- skipEmptyLine,
) where

import Control.Applicative

data LineCount = Wrapper (Int, Int) -- line + column

instance Show LineCount where
    show (Wrapper (l, c)) = " at l:" <> show l <> ", c:" <> show c <> ", "

type Rest = (String, LineCount)

data Parser a = Parser
    { runParser :: Rest -> Either String (a, Rest) }

updateLineCount :: Char -> LineCount -> LineCount
updateLineCount '\n' (Wrapper (l, _)) = Wrapper (l + 1, 0)
updateLineCount _ (Wrapper (l, c)) = Wrapper (l, c + 1)

getRest :: Char -> String -> LineCount -> Rest
getRest c str lc = (str, updateLineCount c lc)

instance Functor Parser where
    fmap fct parser = Parser f
      where
        f r = runParser parser r >>= \(x, r) -> Right (fct x, r)

instance Applicative Parser where
    pure a = Parser f
      where
        f rest = Right (a, rest)
    (<*>) first second = Parser f
      where
        f rest = runParser first rest >>= \(x, rx) -> runParser second rx >>= \(y, ry) -> Right (x y, ry)

instance Alternative Parser where
    empty = Parser f
      where
        f _ = Left ""
    first <|> second = Parser f
      where
        f rest = case runParser first rest of
            result@(Right _) -> result
            _ -> runParser second rest

instance Monad Parser where
    parser >>= fct = Parser f
      where
        f rest = runParser parser rest >>= \(x, r) -> runParser (fct x) r

parserCustomError :: a -> (a -> Parser b) -> String -> String -> Parser b
parserCustomError a p scope detail = Parser $ \rest@(_, lc) -> case runParser (p a) rest of
    Left _ -> Left $ scope <> show lc <> detail
    x -> x

parseChar :: Char -> Parser Char
parseChar c = Parser $ \(s, lc) -> case s of
    [] -> Left $ "Error parsing char \"" <> [c] <> "\"" <> show lc <> "String empty"
    (x : xs)
        | c == x -> Right (x, getRest x xs lc)
        | otherwise -> Left $ "Error parsing char \"" <> [c] <> "\"" <> show lc <> "Char not found"

parseNotChar :: Char -> Parser Char
parseNotChar c = Parser $ \(s, lc) -> case s of
    [] -> Left $ "Error parsing not char \"" <> [c] <> "\"" <> show lc <> "String empty"
    (x : xs)
        | c == x -> Left $ "Error parsing not char \"" <> [c] <> "\"" <> show lc <> "Char found"
        | otherwise -> Right (x, getRest x xs lc)

parseCharAny :: Parser Char
parseCharAny = Parser $ \(s, lc) -> case s of
    (x : xs) -> Right (x, getRest x xs lc)
    [] -> Left $ "Error parsing any char" <> show lc <> "String empty"

parseNotCharAny :: String -> Parser Char
parseNotCharAny s = parserCustomError s f ("Error parsing not char in \"" <> s <> "\"") "Char match element in string"
    where
        f :: String -> Parser Char
        f [] = parseCharAny
        f (x:xs) = parseNotChar x *> f xs


parseString :: String -> Parser String
parseString s = parserCustomError s f ("Error parsing string \"" <> s <> "\"") "String not found"
    where
        f :: String -> Parser String
        f [] = pure []
        f (x : xs) = (:) <$> parseChar x <*> f xs


parseAnyChar :: String -> Parser Char
parseAnyChar s = parserCustomError s f ("Error parsing char in \"" <> s <> "\"") "Char is not matching any of the string"
    where
        f :: String -> Parser Char
        f [] = Parser $ \s -> Left $ "Not found"
        f (x : xs) = parseChar x <|> f xs


skipChars :: String -> Parser String
skipChars s = many (parseAnyChar s)

skipEmptyLine :: Parser String
skipEmptyLine = (parseUntilAnyChar "\n " <* parseAnyChar "\n ")

parseUntilChar :: Char -> Parser String
parseUntilChar c = many $ parseNotChar c

parseInt :: Parser Int
parseInt = read <$> some (parseAnyChar ['0' .. '9'])

parseFloatString :: Parser String
parseFloatString =
    (<>)
        <$> some (parseAnyChar ['0' .. '9'])
        <*> ((:) <$> parseChar '.' <*> some (parseAnyChar ['0' .. '9']))

parseFloat :: Parser Float
parseFloat = read <$> parseFloatString

parseManyWithSeparator :: Parser a -> Char -> Parser [a]
parseManyWithSeparator p c = (:) <$> p <*> many (parseChar c *> p) <|> pure []

parseUntilAnyChar :: String -> Parser String
parseUntilAnyChar str = many $ parseNotCharAny str

parseSomeUntilAnyChar :: String -> Parser String
parseSomeUntilAnyChar str = some $ parseNotCharAny str
