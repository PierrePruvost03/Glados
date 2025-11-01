# Parser

This folder is about parsing our language

## About

Haskell is a very useful language for parsing files. We will only explain our usage of it, not the detailed way it works.

When parsing a file, we expect a well written `.kong` file, but in case it is not, we shall return an error case. This fact is important for future implementation of any syntaxe related feature.

## Structure

The `Parser.hs` file contains our Parser type and all the basic parsing functions to parse specific chains of Characters from a String.

From these atomic operation, we can derive all parsing functions we need, separated into multiple files.

## Parsing into AST

As the name reads, we have an Abstract Syntax Tree (AST) type for our parser to use after reading a `.kong` file. You can find more about this type in the `DataStruct` folder.

This type is made to stock the user's code line by line in a new way. This way doesn't use characters, but types we arbitrarily defined to describe what to do.
