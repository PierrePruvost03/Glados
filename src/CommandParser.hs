module CommandParser
  ( Command,
    parseCommand,
  )
where

import DataStruct.Command
import Parser

parseCommandLine :: Parser Command
parseCommandLine =
  (,)
    <$> some (parseAnyNotChar " \n\t")
    <*> many (skipChars " \t\n" *> some (parseAnyNotChar " \n\t"))

parseCommand :: Parser Command
parseCommand = skipChars " \t\n" *> parseChar ':' *> parseCommandLine
