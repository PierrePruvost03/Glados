# Compiler

This folder is about compiling our language

## About

Compilation of Kong is about turning the AST we got from parsing the `.kong` file into a `.strong` file that our Virtual Machine will be able to read.

## Structure

Compilation happens in the `KongCompiler.hs` file entirely. It is a literal translation from the `AST` type into a list of Instructions, represented by the `Instr` type.

These instructions are the atomic operations our language can execute.

## .strong files

To create our binary files, the `.strong` files, we further translate the list of instructions into bytecode, using the reference described in `VM/Value.hs`.

Those references allow us to easily translate both our list of instructions into a binary file, and our binary file into a list of instructions.
