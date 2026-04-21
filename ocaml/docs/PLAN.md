# Pirx-in-OCaml: Porting Plan

This document is the master plan for rewriting the Pirx compiler in OCaml. It's
a living document — update it as decisions change or milestones complete.

Companion docs (to be written as we go):
- `DESIGN.md` — OCaml-specific design decisions (ADT shapes, error monad, etc.)
- `PROGRESS.md` — running log of what's done, what's in-flight, surprises

## Goals

- Reimplement the Pirx compiler in OCaml with **identical language semantics**.
- Pass the full existing `tests/` suite (104 success tests, ~14 error tests)
  using the existing Go `testrunner`, just pointed at the OCaml-built `pirxc`.
- Keep the compiler architecture close to the Go original. This is a **port**,
  not a redesign. Structural rethinks come after the rewrite lands.

## Non-goals

- No new language features.
- No feature regressions (anything currently passing must still pass).
- No new target platforms. **macOS/arm64 only.** The Linux and x86_64 backends
  are not ported.
- No architectural experiments during the port (no SSA, no new IR, no new
  error model beyond what OCaml naturally affords). Save those for after.

## Key decisions

These were settled during brainstorming and shape everything below:

1. **Hand-written lexer and recursive-descent parser.** No Menhir, no ocamllex.
   Keeps the structure parallel to the Go version and makes it easy to match
   the existing error-reporting style.
2. **Error messages need not be byte-identical to the Go compiler.** "Same
   error, different wording" is acceptable. When we get there, we'll figure
   out a testrunner strategy (most likely: regenerate `.err` fixtures from
   the OCaml compiler and commit them, but decide at M3).
3. **Keep the three-address IR.** Same op set, same semantics. The port stays
   mechanical; we don't want to debug two things at once.
4. **Self-contained OCaml build under `ocaml/`.** The top-level `Makefile`
   doesn't need to learn OCaml during the port. Dune handles everything
   inside the subdirectory.

## Layout

```
pirx/
├── (existing Go tree — untouched during the port)
├── ocaml/
│   ├── dune-project
│   ├── bin/
│   │   └── pirxc/                # main compiler binary (drop-in for Go pirxc)
│   ├── lib/
│   │   ├── lexer/
│   │   ├── parser/
│   │   ├── ast/
│   │   ├── types/                # type system + type table
│   │   ├── typecheck/
│   │   ├── desugar/
│   │   ├── ir/                   # IR defs + generator + optimizer
│   │   └── codegen/              # aarch64-darwin only
│   ├── test/                     # OCaml unit tests (lexer especially)
│   └── docs/
│       ├── PLAN.md               # this file
│       ├── DESIGN.md
│       └── PROGRESS.md
```

The Go `pirx` build driver stays in use; we only replace `pirxc` (the
compile-to-assembly binary). The C stdlib (`stdlib/libpirx.a`) is reused
as-is.

## Testing strategy

- **Primary acceptance gate:** `testrunner testall` green when invoked against
  an OCaml-built `pirxc`. We'll add a make target (or environment variable)
  under `ocaml/` that builds the OCaml `pirxc`, drops it where testrunner
  expects it, and runs the suite.
- **Per-stage unit tests in OCaml** where valuable. Lexer definitely. Parser
  and typechecker probably not — the end-to-end tests exercise them
  thoroughly and unit-testing ADT shapes is low-value.
- **Differential checks as a debugging tool, not a gate.** When something
  breaks mid-port, comparing `go run ./cmd/pirxc -t ast` and `-t ir` output
  against the OCaml compiler's dumps is often the fastest way to localize a
  bug. Don't enforce byte-equality.

## Approach: vertical slice first, then breadth

Rather than porting stage-by-stage (lexer → parser → ... months before
anything runs), each milestone delivers an **end-to-end working compiler for
a growing subset of the language**. This matches the existing test numbering,
which is roughly feature-ordered, and keeps us honest about integration at
every step.

Tests are the acceptance criteria. Each milestone lists which tests must pass
when it's done.

## Milestones

### M0 — Scaffold ✅

Prove the plumbing end-to-end with a trivial compiler that emits an empty .s
file (or a hardcoded one for `000_empty.pirx`).

- `dune-project` + library skeletons under `ocaml/lib/`.
- `ocaml/bin/pirxc` executable that parses flags compatibly with the Go
  `pirxc` (`-o`, `-O0`, `-t`, positional input files). Most flags can be
  no-ops initially.
- `ocaml/Makefile` with targets: `build`, `test`, `clean`, and `install-pirxc`
  (copies the built binary to the repo root as `./pirxc`, replacing Go's).
- CI-local smoke: `make build && ./testrunner test 000` passes.

**Exit criteria:** test `000_empty` passes end-to-end through the OCaml
`pirxc`. **Done** — see `PROGRESS.md`.

### M1 — Hello world slice

Narrowest possible cut through every stage: lexer, parser, typecheck, IR,
codegen for aarch64-darwin. Enough to run `printf("hello\n")`.

- Lexer: identifiers, numbers, strings, keywords needed for tests 000–006,
  operators used by those tests, location tracking.
- Parser: function declarations, extern declarations, call expressions,
  string literals, return statements, blocks.
- Typecheck: function signatures, extern lookup, string-literal type,
  return-type checking for `main(): int`.
- IR: `Call`, `ExternalCall`, `Return`, `ExternalReturn`, `Assign` for
  string literals.
- Codegen: function prologue/epilogue, C-ABI external calls (variadic
  printf!), string literal emission in `.cstring`, `_main` entry.

**Exit criteria:** tests 000–006 pass (`empty`, `putchar`, `function_calls`,
`swap`, `string_literal`, `multiple_string_literals`, `printf`).

Note: `003_swap` uses pointers, which is a stretch for this milestone. If it
gets awkward, push it to M4 and adjust the range.

### M2 — Arithmetic and control flow

- Int arithmetic (`+ - * / %`), comparisons, boolean operators, unary minus,
  unary not, parentheses.
- int64 literals and operations.
- if/else, while, for, break, continue.
- Early return, multiple locals.

**Exit criteria:** tests 007–028 pass. Roughly: arithmetic tests,
`fizzbuzz`, `while_1`, `break`, `continue`, `if_basic`, `else_if`,
`early_return`, `many_locals`.

### M3 — Error reporting, parser + typechecker errors

The first batch of `.err` tests. This is where we settle the error-message
story.

- Parser errors: missing semicolon, `plus_at_eof`, `minus_at_eof`,
  `single_pipe`, etc.
- Typechecker errors: undeclared variables, function check errors, type
  mismatches, return-type mismatches, redeclaration, loop checks,
  initializer errors, invalid lvalues, field access errors, constant errors,
  missing return paths, non-bool for condition.
- Settle: do we regenerate `.err` fixtures via `./testrunner accept`, or
  teach testrunner a "substring match" mode? Default plan: regenerate and
  commit; revisit if the resulting messages look worse than the Go ones.

**Exit criteria:** all `.err` tests pass. Tests affected: 029, 030, 032, 033,
035, 053, 063, 065, 066, 072–075, 082, 084, 097, 101–104, 107.

### M4 — Structs and pointers

- Struct type declarations, field access, nested structs.
- Pointer types, `&`, `*`, pointer arithmetic where the language supports it.
- Heap alloc via `new(T)` lowering to `PirxAlloc`, `dispose()`.
- Type table integration.

**Exit criteria:** struct- and pointer-heavy tests in the mid-range pass.
Includes `003_swap` if it was deferred from M1.

### M5 — Slices

- Slice type `[]T`, `new([]T, n)`, `resize`, `getptr`, `getsize`, `getcap`.
- Slice indexing, slice-of-slice ranges.
- Correct ABI for `PirxSlice` (16-byte struct: ptr + size + cap).
- `string` is a slice of int8 in the current implementation — make sure
  string literals lower consistently.

**Exit criteria:** tests 090, 092, and all slice-using tests pass.

### M6 — int8 and int64 mechanics, numeric casts

- int8 load/store with correct widths (the `strb` vs `str` regression from
  test 105).
- Sign-extension on int8 → int (test 094).
- Cross-type casts (test 091).

**Exit criteria:** tests 091, 094, 096, 105 pass, plus anything else relying
on sub-word integer mechanics.

### M7 — Floats

- float32 / float64 types, literals, arithmetic, comparison, unary negation.
- Float register allocation in codegen (aarch64 d0–d7 for varargs on Darwin,
  etc.).
- Float <-> int casts.

**Exit criteria:** tests 089, 099, 100 pass.

### M8 — Remaining features

Sweep whatever's left:

- File I/O (`PirxOpen`, `PirxReadLine`, `PirxClose`) — test 095.
- Variadic external polish as needed (test 088).
- `for` loop corner cases (infinite loop, non-bool condition) — tests 106,
  107.
- Compound assignment (test 093).
- Char-indexing into strings (test 096).
- Postfix `++`/`--` as statement (note: `TODO.md` documents a known bug with
  expression use — preserve the bug, don't fix it during the port).
- Short-circuit `&&`/`||` (another `TODO.md` bug — preserve as-is).

**Exit criteria:** all 104 success tests + all error tests pass against the
OCaml `pirxc` at default optimization.

### M9 — IR optimizer

- Port `internal/ir/optimize.go` (644 lines — constant folding, dead code,
  whatever's there).
- Port any aarch64 peephole passes from `internal/codegen/aarch64/optimize.go`.

**Exit criteria:** `testrunner testall -O0` *and* default (`-O1`) both green.
CI target equivalent of Go's `citest` passes.

### M10 — Drop-in swap

- Top-level `Makefile` learns `make ocaml-pirxc` or similar to build the
  OCaml compiler and use it as `./pirxc`.
- Decide whether to keep the Go compiler buildable in parallel, or remove it
  (probably keep for one release, then delete — decide at M10).
- README updated to describe the OCaml build.

**Exit criteria:** fresh clone → `make` → `make test` green, using OCaml
`pirxc`.

## Bugs to preserve

Per `TODO.md`, the current compiler has known bugs. We **preserve** these
during the port (same behavior) and track them for post-port fixing:

- Postfix `++`/`--` returns post-increment value instead of pre-increment
  when used as an expression.
- `&&` / `||` don't short-circuit.

Fixing them during the port would mean behavior change — that's a separate
workstream. If `TODO.md` acquires new entries during the port (e.g.,
"OCaml version diverges from Go version here"), note them there too.

## Deferred / explicitly out-of-scope

- Linux targets (aarch64 and x86_64).
- Darwin x86_64.
- Rewriting the stdlib in anything other than C.
- Rewriting the `pirx` build driver.
- Rewriting `testrunner`.
- Any new language features or bug fixes.

## Working style

From `CLAUDE.md` and confirmed by the user: small iterations, run tests
frequently. Concretely:

- Pick one milestone; within it, add features one test at a time.
- After each feature: run `./testrunner test NNN` for the targeted test, and
  `testall` for the growing passing subset, before moving on.
- Keep `PROGRESS.md` updated so it's obvious where we are mid-milestone.
- When a decision surfaces that affects design broadly (error monad shape,
  how we represent types, IR op encoding), record it in `DESIGN.md` with a
  short rationale.
