module AstParsing.Skip
  ( skip,
  )
where

import Parser

skip :: Parser String
skip = skipChars " \n\t"
