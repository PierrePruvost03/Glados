module Parser (
    Parser,
    LineCount,
    Rest,
    ParsingError,
    getLineCount,
    generateError,
    runParser,
    parseChar,
    parseEmpty,
    parseString,
    parseInt,
    parseFloat,
    parseCharAny,
    parseAnyChar,
    parseNotChar,
    skipChars,
    skipSomeChars,
    parseUntilChar,
    parseManyWithSeparator,
    parseAnyNotChar,
    parseUntilAnyChar,
    fmap,
    pure,
    many,
    some,
    (<*>),
    empty,
    (<|>),
    (>>=),
    parseSomeUntilAnyChar,
) where

import Control.Applicative

type LineCount = (Int, Int) -- line + column

type Rest = (String, LineCount)

type ParsingError = (String, String, LineCount) -- context, error, LineCount

newtype Parser a = Parser
    { runParser :: Rest -> Either ParsingError (a, Rest) }

updateLineCount :: Char -> LineCount -> LineCount
updateLineCount '\n' (l, _) = (l + 1, 0)
updateLineCount _ (l, c) = (l, c + 1)

getRest :: Char -> String -> LineCount -> Rest
getRest c str lc = (str, updateLineCount c lc)

instance Functor Parser where
    fmap fct parser = Parser f
      where
        f r = runParser parser r >>= \(x, rest) -> Right (fct x, rest)

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
        f _ = Left ("Empty", "Empty", (0, 0))
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
    Left _ -> Left (scope, detail, lc)
    x -> x

getLineCount :: Parser LineCount
getLineCount = Parser $ \rest@(_, lc) -> Right (lc, rest)

generateError :: String -> String -> Parser a
generateError c d = Parser $ \(_, lc) -> Left (c, d, lc)

parseChar :: Char -> Parser Char
parseChar c = Parser $ \(s, lc) -> case s of
    [] -> Left $ ("Error parsing char \"" <> [c] <> "\"", "String empty", lc)
    (x : xs)
        | c == x -> Right (x, getRest x xs lc)
        | otherwise -> Left ("Error parsing char \"" <> [c] <> "\"", "Char not found", lc)

parseEmpty :: Parser String
parseEmpty = Parser $ \(s, lc) -> case s of
    [] -> Right ("", ("", lc))
    _ -> Left ("Error parsing empty", "string not empty", lc)

parseNotChar :: Char -> Parser Char
parseNotChar c = Parser $ \(s, lc) -> case s of
    [] -> Left ("Error parsing not char \"" <> [c] <> "\"", "String empty", lc)
    (x : xs)
        | c == x -> Left ("Error parsing not char \"" <> [c] <> "\"", "Char found", lc)
        | otherwise -> Right (x, getRest x xs lc)

parseCharAny :: Parser Char
parseCharAny = Parser $ \(s, lc) -> case s of
    (x : xs) -> Right (x, getRest x xs lc)
    [] -> Left ("Error parsing any char", "String empty", lc)

parseAnyNotChar :: String -> Parser Char
parseAnyNotChar str = Parser $ \(s, lc) -> case s of
    [] -> Left ("Error parsing any not char \"" <> str <> "\"", "String empty", lc)
    (x : xs)
        | x `elem` str -> Left ("Error parsing any not char \"" <> str <> "\"", "Character found in string", lc)
        | otherwise -> Right (x, getRest x xs lc)

parseString :: String -> Parser String
parseString s = parserCustomError s f ("Error parsing string \"" <> s <> "\"") "String not found"
    where
        f :: String -> Parser String
        f [] = pure []
        f (x : xs) = (:) <$> parseChar x <*> f xs


parseAnyChar :: String -> Parser Char
parseAnyChar str = parserCustomError str f ("Error parsing char in \"" <> str <> "\"") "Char is not matching any of the string"
    where
        f :: String -> Parser Char
        f [] = Parser $ \(_, lc) -> Left ("Error parsing any char", "char not found", lc)
        f (x : xs) = parseChar x <|> f xs


skipChars :: String -> Parser String
skipChars s = many (parseAnyChar s)

skipSomeChars :: String -> Parser String
skipSomeChars s = some (parseAnyChar s)

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
parseUntilAnyChar str = many $ parseAnyNotChar str

parseSomeUntilAnyChar :: String -> Parser String
parseSomeUntilAnyChar str = some $ parseAnyNotChar str
