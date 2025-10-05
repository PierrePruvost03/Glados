# GLaDOS

Welcome to GLaDOS Part 1

## About

This is the first part of the GLaDOS, a lisp interpretter.

To see [official lisp documentation](https://lisp-docs.github.io/cl-language-reference/chap-2/c-b-character-syntax)
> [!NOTE]
> Keep in mind that this is a minimalist lisp interpretter, all the supported features are listed [here](#supported-features)

## How to build

`make`
`./glados`

## Supported features

### Types

- Integers -> `12` and `-12`
- Booleans -> `#t` and `#f`*
- Strings -> `"this is a string"`
- Lists -> `(define string "This is a list conataining two symbols and a string")`

### Keywords

- 'define' -> `(define var "value")`
- 'lambda' -> `(define function (lambda (x) x))`
- 'if' -> `(if (zero? 1) "1 is zero" "1 is not zero")`

### Primitives

- '+' -> `(+ 2 3) ; 5`
- '-' -> `(- 3 2) ; 1`
- '*' -> `(* 2 3) ; 6`
- 'div' -> `(div 6 2) ; 3`
- 'eq?' -> `(eq? 6 2) ; #f`
- '<' -> `(< 3 6) ; #t`
- '>' -> `(> 3 6) ; #f`
- 'mod' -> `(mod 19 6) ; 1`
- 'zero?' -> `(zero? 1) ; #f`

## Run tests

### Unit tests

`make tests_run`

### Functional tests

`make functional_tests_run`

## Commands

while using the interpretter in **standard input** mode, you can type some commands:

- `:help` -> show the list of the available commands
- `:load <file>...` -> execute a file in the interpreter, loading his symbols
- `:type [<symbol>...]` -> get the type of a symbol (or all of them if no specified)

## Contributors
| Pierre Pruvost | Abel Daverio | Alexandre Guillaud | Sami Hamrouni | Paul Berlioz |
|--|--|--|--|--|
| <img src="https://github.com/PierrePruvost03.png" width="150em"/> | <img src="https://github.com/abeldaverio.png" width="150em"/> | <img src="https://github.com/LixiosDelios.png" width="150em"/> | <img src="https://github.com/PouletHalal.png" width="150em"/> | <img src="https://github.com/PoloTheAspicot.png" width="150em"/> |
