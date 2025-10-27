# Kong

Welcome to Kong source files

## About

These files are the working force of the project. If you are just a Kong user, **no need to pay attention to them**.

We give them to contribution, as you will find some help to understand our overall project architecture further in the folders.

## Architecture

### Parser

The first step into our language's journey is the Parser. It allows us to read user's `.kong` files and transform them into an Abstract Syntax Tree (AST), which is an organized list representing the user's code.

You will find our more about it in the `src/AstParsing` folder.

### Compiler

The second step is the compiler. It takes the AST and translate it into bytecode, which is a further decomposed list of instructions. Generated file uses the `.strong` extension format.

You will find our more about it in the `src/Compiler` folder.

### Virtual Machine

The last step is the Virtual Machine. It reads a `.strong` file and uses its bytecode to execute the program.

You will find our more about it in the `src/VM` folder

### Types and structures

During your journey in our code, you will encounter special types we specially defined for this usage.

You can find their corresponding definition on the `src/DataStruct` folder.
