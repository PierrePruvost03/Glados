module AstParsing.Utils where

import Parser
import DataStruct.Ast
import AstParsing.Keywords.Keywords
import AstParsing.Skip

parseName :: Parser String
parseName = parseUntilAnyChar " \n\t;"
