# Pirx-in-OCaml: Progress Log

Running notes on what's been done, what's in-flight, and anything surprising
along the way. Oldest entries at the top.

## M0 — Scaffold ✅

Goal: prove end-to-end plumbing (dune build → install → testrunner) with a
trivial compiler.

Done:
- `ocaml/dune-project` (lang dune 3.16).
- `ocaml/bin/pirxc/` — single-file executable (`main.ml`) that parses the Go
  `pirxc` flag surface (`-o`, `-O0`, `-t`, positional inputs) and emits a
  hardcoded aarch64-darwin `.s` file. Input files are not read.
- `ocaml/Makefile` with `build`, `test`, `clean`, and `install-pirxc`. Default
  target runs `build` then `install-pirxc`, which copies
  `_build/default/bin/pirxc/main.exe` to `../pirxc` (replacing the Go
  binary — reversible via a top-level `make pirxc`).
- Smoke test: `./testrunner test 000` passes against the OCaml-built `pirxc`.

Environment notes:
- OCaml toolchain is via opam, default switch = OCaml 5.3.0, dune 3.19.1. Need
  `eval $(opam env)` before running `make` (not persisted in the user shell).
- `./pirxc` is gitignored, so overwriting is safe and recoverable.

Not yet addressed (intentionally — deferred to later milestones):
- No library skeletons under `ocaml/lib/` yet. They'll be created as real
  code lands (M1 onward), to avoid empty placeholder dirs.
- No OCaml unit-test framework chosen yet. `dune runtest` is wired up but
  has nothing to run. Will pick (likely `alcotest`) when the lexer arrives.
- Top-level `Makefile` does not yet know about OCaml. Integration is M10.
- Linter/formatter (`ocamlformat`) not configured. Add when the codebase is
  large enough to care about style drift.

## M1 — Hello world slice

### S1 — Library skeleton + lexer ✅

Done:
- `lib/location/` — `Location.t = { file; line; col }` with `sexp`/`compare`/
  `equal` derivers and a `to_string` ("file:line:col").
- `lib/lexer/` — `Token.t` and `Lexer.t`. Lexer is a string-backed cursor,
  not a buffered reader (simpler and fine for the source sizes we handle).
  Covers the subset needed by tests 000–006: three keywords (`func`/`var`/
  `return`), decimal numbers, strings with `\n \t \r \\ \" \'` escapes,
  `=`, `( ) { } , : ;`, `//` line comments. Unknown chars and
  unterminated strings raise `Lexer.Compile_error`.
- `test/lexer/` — 13 alcotest cases covering each token class, comment
  skipping, location tracking, 000/003 fixture token streams, and both
  error paths. `dune runtest` green.
- `./testrunner test 000` still passes (M0 hardcoded path untouched).

Deviations from DESIGN.md logged:
- **Token shape.** DESIGN.md M1.2 originally specified string-tagged
  `Tok_keyword of string` / `Tok_op of string` / `Tok_punct of string`.
  Switched to exhaustive constructors (`Kw_func`, `Op_assign`, `Lparen`,
  …) so the parser gets exhaustiveness checks. DESIGN.md M1.2 updated to
  reflect the new shape.
- **Lib placeholders.** DESIGN.md §S1 suggested creating all eight
  `lib/*/` subdirs up front with dune stubs. Skipped — only `location/`
  and `lexer/` exist. Matches the PROGRESS.md M0 note ("create as code
  lands"). Later stages will add their own subdir when their session
  arrives.
- **No `public_name` on libraries.** The `pirx.*` public names dune
  expected require a `pirx.opam` at the project root, which we don't
  have and don't want yet. Dropped to internal-only `(name pirx_<stage>)`.

Environment notes for next sessions:
- `alcotest` 1.9.1 was installed into the opam switch. `fmt` has to be
  named explicitly as a library dep even though alcotest depends on it
  (its cmi isn't transitively in-scope when `implicit_transitive_deps`
  is off).

Not yet addressed (intentionally — deferred to later S-sessions):
- Keywords/operators beyond what tests 000–006 need (`if`, `else`, `for`,
  arithmetic, comparisons, …). They arrive with their tests.
- Char literals, hex numbers, `l`/`i8`/float literals. Later milestones.

### S2 — AST + parser + `-t ast` ✅

Done:
- `lib/types/` — `Type.t` ADT with `Int`/`Int8`/`Int64`/`Bool`/`String`/
  `Void`/`Undefined`/`Pointer`/`Slice` and `to_string` producing Go-
  compatible names (`*int`, `[]int`, etc.).
- `lib/ast/` — `expr`/`stmt`/`func`/`program` ADTs per DESIGN.md §M1.2,
  plus a hand-written S-expression printer in `Ast.Pp`. Printer output is
  byte-identical to `go run ./cmd/pirxc -t ast` for tests 000–006.
  `escape_string` ported from Go's `internal/util/strings.go:EscapeString`.
- `lib/parser/` — recursive-descent parser with a one-slot peek buffer.
  Covers the M1 subset: top-level `func`, param list, return type,
  block body, `var`-decl (type and/or initializer), assignment, `return`,
  expression statement, and primary expressions (int lit, string lit,
  identifier, function call). No operator precedence yet — `parse_expression`
  is just `parse_primary`; the climber arrives in M2.
- `bin/pirxc` wired for `-t ast`: reads the input file, lexes, parses,
  prints. Other targets still emit the M0 hardcoded blob so
  `./testrunner test 000` stays green. Output extension for text dumps
  (`ast`/`final_ast`/`ir`) is `.txt`; assembly targets still use `.s`.
  `Lexer.Compile_error` is caught in `main.ml` and printed as
  `file:line:col: msg` before `exit 1`.
- `test/parser/` — three alcotest cases: printer pins for 000 and 004,
  plus one negative case (missing `;` raises `Compile_error`). `dune
  runtest` green.

Deviations from DESIGN.md / plan:
- **`func.body : stmt list`**, not a `Block` record. Go wraps it in
  `*Block{Loc,Statements}` but the loc isn't used by anything we
  implement in M1. Printer still emits `(block …)` unconditionally.
  Revisit when M2 nested blocks arrive.
- **`func.args : (string * Type.t) list`**, no per-arg location record.
  Go's `Arg{Loc,Name,Type}` carries a loc we don't use yet.
- **`Compile_error` still lives in `Pirx_lexer.Lexer`.** Parser re-raises
  it. DESIGN.md D2 just requires a shared exception; introducing a
  dedicated `pirx_errors` lib for one constructor felt like overkill.
  Move if/when the typechecker wants in.
- **Output file extension for text dumps.** `-t ast -o foo.pirx` without
  `-o` now writes `foo.txt` instead of `foo.s`. The Go driver doesn't
  care because it always uses `-o -` for dumps; harmless for us too.
- Added `test/parser/` despite DESIGN.md's "no unit tests below lexer"
  rule. Two pinning cases + one error case — cheap insurance against
  printer drift. Will skip unit tests for typechecker/ir as planned.

Environment notes for next sessions:
- Output of `./pirxc -t ast -o - <file>` now mirrors
  `go run ./cmd/pirxc -t ast -o - <file>` for tests 000–006. Use `diff`
  between them as a debug aid when S3/S4 land.

Not yet addressed (deferred to S3+):
- Typechecker (no `typ` filled on expressions yet — always `Undefined`).
- Desugaring of string literals into `PirxString`.
- IR generation and codegen wiring.

### S3–S7

Not started.
