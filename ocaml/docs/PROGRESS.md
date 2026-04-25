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

### S3.1 — Typechecker ✅

Done:
- `lib/typecheck/` with:
  - `Diag` — mutable `entry list` collector; `push`/`pushf`/`entries`/`is_empty`
    per DESIGN.md D2.
  - `Varstack` — frame stack of `(string * Type.t) list`. Innermost-first
    `lookup`; `declare` returns `` `Ok | `Duplicate_in_scope ``.
  - `Proto.t` — shared record for builtin + user-function signatures.
    `external_name` is carried but unused until IR-gen.
  - `Builtins.protos` — hardcoded M1 table: `PirxString`, `printf` (variadic,
    external name `PirxPrintf`), `putchar`.
  - `Functions.build` — seeds a `String.Table.t` with builtins, folds in
    user functions via `Hashtbl.set` (last write wins). User functions
    silently shadow builtins; duplicate user functions silently overwrite
    the earlier entry. Matches Go's `declaredFuncs[name] = proto`
    semantics — the Go typechecker doesn't warn about duplicates at this
    stage either.
  - `Typecheck.check : Ast.program -> Ast.program * Diag.t` — mutates
    `expr.typ` in place. On error: `Diag.push` + set `typ = Undefined`,
    continue walking. A `compatible : Type.t -> Type.t -> bool` helper
    treats `Undefined` on either side as "already errored, don't cascade,"
    keeping every call site honest about the rule.
- `Typecheck.check_func` opens two scopes per function: one for the
  params, then a nested one for the body. Matches Go
  (`checkFunction` + `checkBlock`) so that `func f(x: int) { var x: int = 1; }`
  shadows rather than collides.
- `test/typecheck/` — eleven alcotest cases: four positives
  (000/002/003/006 parse+check clean, plus spot-checks that specific call
  results are typed correctly), two scope pins (param shadowed by
  body-local, user function shadowing a builtin), and five negatives
  (undeclared ident, arity mismatch, init-type mismatch, void return with
  required value, duplicate `var` in same scope). Negative assertions use
  substring match on diag messages, per D2's "same error, different
  wording OK" policy.
- `./testrunner test 000` still passes — main.ml is untouched this
  session.

Deviations from DESIGN.md:
- **`Hashtbl.create (module String)`** hits a `Base`/`Core` signature
  mismatch in this toolchain (`Base_internalhash_types.hash_value` vs
  `int`). Switched to `String.Table.create ()` — same effect, Core-native.
- **`Proto` lifted to its own module** (`proto.ml`) so `builtins.mli` and
  `functions.mli` can both reference the type without cycles. DESIGN.md
  didn't spell out the split; this is the obvious one.
- **No `Proto.mli`.** The record is exposed directly; fields are all that
  consumers need. Add one if the type grows private state.
- **Return-type check messages** say "must return a value of type X" and
  "returned value has type X, expected Y" — worded for humans rather
  than matching Go verbatim. D2 allows this; `.err` fixtures will be
  regenerated at M3.

Environment notes for next sessions:
- The full dune test set is now `lexer` + `parser` + `typecheck`. Run
  `eval $(opam env) && dune runtest` to catch regressions across all three.
- `Typecheck.check` is ready for S3.2 to call — just thread the returned
  `Diag.t` into a "non-empty → print and exit 1" path in `main.ml`.

Not yet addressed (deferred to S3.2+):
- `main.ml` wiring — `-t final_ast` pipeline and diag-to-stderr on
  non-empty lands in S3.2.
- Desugar of string literals to `PirxString` — S3.2.
- Builtins beyond M1's three — each later milestone grows the table.
- No `is_lvalue` beyond `E_ident`; field/deref targets arrive with M4.

### S3.2 — Desugar + `-t final_ast` ✅

Done:
- `lib/desugar/` with `Desugar.run : Ast.program -> Ast.program`.
  - Single M1 rule: `E_string_lit s` rewritten to
    `E_call "PirxString" [E_int_lit (byte_len s); E_string_lit s]` with
    outer call typed `String`, inner int literal typed `Int`. Inner
    `E_string_lit` passes through unchanged with its `typ = Type.String`
    from S3.1.
  - Recurses into `E_call` args, `S_var_decl` init, `S_assign` target/value,
    `S_return` value.
- `bin/pirxc`:
  - Added `-t final_ast` target: lex → parse → typecheck → desugar → print.
  - On non-empty `Diag`: prints all entries to stderr as `file:line:col: msg`,
    then `exit 1`.
  - Hardcoded asm updated from aarch64-darwin (`;` comments) to aarch64-linux
    (`//` comments, no `#` on immediates, no `_` prefixes, `.type` directives).
    Default target changed from `"aarch64-darwin"` to `"aarch64-linux"` to
    match the actual build platform.
- `dune runtest` green (27 tests: 13 lexer, 3 parser, 11 typecheck).
- `./testrunner test 000` passes.

Deviations from DESIGN.md:
- **Hardcoded asm updated to aarch64-linux.** M0–S2 ran on macOS and used
  Darwin-style assembly (`;` comments, `_` prefixes, `#` on immediates).
  On this Linux host that assembly failed GAS. Updated to match
  `go run ./cmd/pirxc` on aarch64-linux. S5 will replace the blob with
  real codegen that emits the correct format per platform.
- **Dual-target scope change.** PLAN.md originally said "macOS/arm64 only";
  updated in this session to target both aarch64-darwin and aarch64-linux.
  DESIGN.md M1.6 and S5/S6 updated accordingly.

Not yet addressed (deferred to S4+):
- IR generation (`-t ir`).
- Codegen wiring (S5–S6).

### S4 — IR generation + `-t ir` ✅

Done:
- `lib/ir/` — `Ir.arg`, `Ir.call_arg`, `Ir.op`, `Ir.func`, `Ir.program` ADTs
  matching Go's IR types (M1 subset: `Assign`, `Call`, `ExternalCall`,
  `Return`, `ExternalReturn`).
- `lib/ir/pp.ml` — printer matching Go's `IrProgram.Print` / `IrFunction.Print`
  format byte-for-byte. `count_locals_and_temps` mirrors Go's logic
  (locals = non-`$` non-`@` targets; temps = `$`-prefixed targets).
- `lib/ir/generator.ml` — `Generator.generate : Ast.program -> Ir.program`.
  - Builds function table via `Functions.build` (reuses typechecker infra).
  - Per-function: tracks locals/temps, renames `main` → `Pirx_Main`, emits
    implicit bare return if function doesn't end with `Return`/`ExternalReturn`.
  - Expression lowering: `E_int_lit` → `Int_lit` (size 4), `E_string_lit` →
    `String_lit` (size 8, matching Go's hardcoded literal size),
    `E_ident` → `Var`, `E_call` → `Call` or `ExternalCall` with temp
    allocation for non-void returns.
  - Statement lowering: `S_var_decl` (with/without init), `S_assign` (ident
    targets only for M1), `S_expr`, `S_return`.
  - Emits synthetic `Pirx_Init` (empty `Return()`) and `main` wrapper
    (`Call Pirx_Init`, `Call4($ret = Pirx_Main)`, `ExternalReturn4($ret)`).
- `bin/pirxc` wired for `-t ir`: lex → parse → typecheck → desugar → ir-gen →
  print. Hardcoded asm fallback still active for default target.
- `test/ir/` — 5 alcotest cases: empty, putchar, string literal (PirxString +
  PirxPrintf), function calls (temp allocation + Assign), swap (multiple
  locals, no temps).
- IR output diffed against `go run ./cmd/pirxc -O0 -t ir` for tests 000–006:
  **all diffs empty** (exact match on op set, sizes, argument order; temp
  naming is `$1`, `$2`, … which matches Go's `-O0` output).
- `dune runtest` green (32 tests: 13 lexer, 3 parser, 11 typecheck, 5 ir).

Deviations from DESIGN.md:
- **No `next_label` in generator state.** M1 has no control flow, so labels
  are unused. Field omitted to avoid unused-field warnings; will add at M2.
- **IR generator depends on `pirx_typecheck`.** DESIGN.md D7 says one library
  per stage. `pirx_ir` reuses `Functions` and `Proto` from `pirx_typecheck`
  rather than duplicating the function-table builder. No cycle introduced.

Environment notes for next sessions:
- `./ocaml/_build/default/bin/pirxc/main.exe -t ir -o - tests/NNN_*.pirx`
  now produces output identical to `go run ./cmd/pirxc -O0 -t ir` for
  tests 000–006. This is the primary debug aid for S5/S6 codegen.

Not yet addressed (deferred to S5+):
- Codegen (real assembly emission). Default target still uses M0 hardcoded
  blob, so `./testrunner test 000` passes but 001–006 do not yet.

### S5–S7

Not started.
