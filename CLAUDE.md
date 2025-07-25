# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Key Rules

__Always follow those unless I explicitly tell you otherwise!__

* You're helping me implement a compiler for a small programming language called Pirx.
* You will only work on tasks I explicitly ask you to do. When you finish the task I asked you to do, simply stop and say you're done. Don't give me the useless summary or any other nonsence.
* Don't ever remove existing unit tests. Ever!
* Work in small iterations, run tests frequently.
* If you need examples of the Pirx language, look in the `tests/` directory. Ignore the `examples/` directory.
* Once you're done with your task, run go fmt like so: `go fmt ./...` to format all files.

## Compiler Architecture

The Pirx compiler follows a traditional multi-stage compilation pipeline:

1. **Lexer** (`internal/lexer/`) - Tokenizes source code into lexemes
2. **Parser** (`internal/parser/`) - Builds Abstract Syntax Tree (AST) from tokens
3. **Type Checker** (`internal/checks/`) - Performs semantic analysis and type checking
4. **IR Generator** (`internal/ir/`) - Converts AST to three-address intermediate representation
5. **Code Generator** (`internal/codegen/`) - Generates target-specific assembly code

The main compiler entry point is `cmd/pirx/main.go` which orchestrates these stages. You can output intermediate representations using `-t ast` or `-t ir` flags.

## Building and Testing

### Main build tool - make

- `make` or `make pirx testrunner` - Build compiler and test runner
- `make test` - Run Go unit tests and all integration tests. __This is your main tool, run it frequently__.
- `make clean` - Clean build artifacts
- `make stdlib` - Build standard library (libpirx.a)

### End-to-end tests

End-to-end tests are in `tests/` directory with naming convention `NNN_description.pirx`:
- `.pirx` files contain Pirx source code
- `.out` files contain expected program output (success tests)
- `.err` files contain expected compiler error messages (error tests)

The testrunner automatically discovers tests and determines whether they're success or error tests based on which reference file exists.

_Note that all end-to-end tests also run when you execute `make test`._

You can run individual end-to-end tests like so:

* `./testrunner test NNN` - execute the end-to-end test and compare the result with the reference (.out or .err file).
* `./testrunner run NNN` - execute the test and print its output to stdout/stderr without comparing to the reference. Use this when you want to see the output of a test program.
* `./testrunner accept NNN` - execute the test and persist its output to the .out or .err file automatically. Use this only after running the test via `./testrunner run` and double-checking that its output exactly matches your expectations.

### Running the compiler stages

You can also run individual stages for the compiler directly:

* `go run ./cmd/pirx -t ast -o - /tmp/example.pirx` generate AST and print it to stdout in S-expression notation.
* `go run ./cmd/pirx -t ir -o - /tmp/example.pirx` generate IR
* `go run ./cmd/pirx -t ir -o - /tmp/example.pirx` generate assembly for the default platform

## Language Features

Pirx is a C-like systems programming language supporting:
- Functions with typed parameters and return values
- Basic types: int, int64, int8, bool, pointers, structs
- Control flow: if/else, while loops, break/continue
- Extern functions for C interop
- Heap allocation and pointer manipulation
- String literals and printf-style formatting

## Key Components

- `internal/ast/` - AST node definitions and types
- `internal/types/` - Type system and built-in types
- `internal/util/` - Utility functions for bit operations, pointers, strings
- `stdlib/` - C standard library integration (builtin.c/h)
- `cmd/testrunner/` - Comprehensive test runner with parallel execution support
