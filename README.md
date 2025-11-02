# Kong

Welcome to Kong, a GLaDOS project

## About

This is the second part of the GLaDOS EPITECH project.

The first part consisted in the creation of a lisp interpretter.
You can find it in the `ListInterpreter` folder.

This second one sees us creating our own programming language, the Kong.
You can find its source files in the `Kong` folder.

## How to install

You can find the 2 binaries in the last release.

## Supported features

See the [Kong Wiki](#) for a full list of supported features and examples.

### Types

#### Numbers

- **Int**
- **Khar** (character type)
- **Float**
- **Bool**

#### Wrapper

- **Vector** — e.g. `Int<i>` (Int vector of size *i*)
- **Array** — e.g. `Int[3]` (Int array of size 3, only constant values)
- **Tuples** — e.g. `|Int, Float|` (tuple of Int and Float)
- **String** — alias for `Khar<>`

#### Custom

- **Strukt** — e.g. `Strukt Person { Int age; String name }`
- **Typedefs** — e.g. `'Type NewType = Int;'`

### Keywords

- Funk
- Strukt
- Type
- Inklude
- Konst
- Kong
- Strong
- Int
- Float
- Bool
- Khar
- String
- True
- False
- If
- Elif
- Else
- For
- In
- Return
- While
- Trait
- Impl
- self
- Self
- Kast

### Primitives Infixes

- **+**
- **-**
- **/**
- **\***
- **%**
- **\`>\'**
- **\`>=\'**
- **\`<\'**
- **\`<=\'**
- **\`||\'**
- **\`&&\'**

## Run tests

`make test`

### Functional tests

`make functional_tests_run`

## Contributors
| Pierre Pruvost | Abel Daverio | Alexandre Guillaud | Sami Hamrouni | Paul Berlioz |
|--|--|--|--|--|
| <img src="https://github.com/PierrePruvost03.png" width="150em"/> | <img src="https://github.com/abeldaverio.png" width="150em"/> | <img src="https://github.com/LixiosDelios.png" width="150em"/> | <img src="https://github.com/sami-hmr.png" width="150em"/> | <img src="https://github.com/PoloTheAspicot.png" width="150em"/> |
