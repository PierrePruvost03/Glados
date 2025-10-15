module AstParsing.Skip where

import Parser

skip :: Parser String
skip = skipChars " \n\t"
