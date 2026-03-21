---
name: pirx-test
description: Work with Pirx compiler end-to-end tests — explore, run, update, and create tests.
user_invocable: true
---

# Pirx Test Skill

You are helping work with end-to-end tests for the Pirx compiler. Tests live in the `tests/` directory.

## Test structure

- Each test has a numeric prefix like `NNN_description` (e.g. `096_string_indexing`)
- **Single-file tests**: a `.pirx` source file in `tests/`
- **Directory tests**: a directory `tests/NNN_description/` containing multiple `.pirx` and/or `.c` files
- **Expected output**: `tests/NNN_description.out` for success tests
- **Expected errors**: `tests/NNN_description.err` for error tests (compilation should fail)
- A test may also have auxiliary files like `.txt` input files

## Available commands

The test runner binary is `./testrunner` (must be built first). The Makefile provides convenient targets:

- `make test` — build everything and run all tests (Go unit tests + end-to-end tests)
- `make test_NNN` — build everything and run a single test (e.g. `make test_096`)
- `./testrunner test NNN` — run a single test and compare output (compiler must already be built)
- `./testrunner run NNN` — run a test and print output without comparing (for inspection)
- `./testrunner accept NNN` — run a test and save output to `.out` or `.err` file
- `./testrunner testall` — run all tests

**Important**: `make test_NNN` rebuilds the compiler first, while `./testrunner` commands use whatever `./pirx` binary already exists. When in doubt, use the make target.

## How to handle each task

### Exploring tests

List tests with: `ls tests/*.pirx tests/*/` or use Glob patterns. To see what a specific test does, read its `.pirx` file and corresponding `.out` or `.err` file.

### Running tests

- To run a specific test: `make test_NNN`
- To run all tests: `make test`
- To see test output without comparison: first build with `make`, then `./testrunner run NNN`

### Updating tests (accepting new output)

When the compiler behavior changes and a test's expected output needs updating:

1. First run the test to see the new output: `make` then `./testrunner run NNN`
2. Verify the new output is correct
3. Accept: `./testrunner accept NNN`
4. Verify the update: `./testrunner test NNN`

### Creating new tests

1. Find the next available test number by listing existing tests
2. Create the `.pirx` source file(s) in `tests/`:
   - For single-file tests: `tests/NNN_description.pirx`
   - For directory tests: `tests/NNN_description/` with source files inside
3. Build the compiler: `make`
4. Run the test to check output: `./testrunner run NNN`
5. If the output is correct, accept it: `./testrunner accept NNN`
6. Verify: `./testrunner test NNN`

For error tests (tests that should fail compilation), the same workflow applies — `accept` will automatically create a `.err` file if compilation fails.
