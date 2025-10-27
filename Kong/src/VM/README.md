# Virtual Machine

This folder is about the Virtual Machine of our language

## About

The Virtual Machine (VM) part of a language is were the execution of the program takes place. It executes one of our binary file, defined by the `.strong` extension.

## Structure

Execution primarily takes place in the `Execution.hs` file. After translating the binary file into a list of `Instr` again, we stock it in a VMState before executing instructions one by one.

## The VMState data

Our VM uses the principle of a stack. A stack is a list of values one on top of another. When we add an element in the stack it goes on the top, and when we take an element from the stack it comes from the top.

In addition to the stack, which always change, we need some more permanent ways to stock function declarations and global variable. That is why the VM also has an environment and a heap.

To keep track of the execution, the list of instruction is stocked as the code data. We navigate this data using an Instruction Pointer (IP) that helps us going forward or backward when executing the program.
