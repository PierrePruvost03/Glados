# Data Structure

This folder is about all Datas and Structures we use in our source files.

## About

This folder is used in all three parts on the source code. Some datas are transversal and used in multiple parts, explaining generic names and code.

## Structure

The `Bytecode` folder describes all datas used by the Virtual Machine that you can find in the `VM` folder. It also stocks the codes used by the compiler, in the `Compiler` folder to make a `.strong` file or read from it.

Meanwhile, the `AST.hs` file describes how our Abstract Syntax Tree data works for the parser and the compiler, respectively in the `AstParsing` and `Compiler` folders. It is made to stock the user's code line by line in a way that best fits our usage.

Finaly, the `VM.hs` file is used to define the VMState data used by the compiler and the VM, in the `Compiler` and `VM` folders. It describes how the VMState data works and must be used accordingly.
