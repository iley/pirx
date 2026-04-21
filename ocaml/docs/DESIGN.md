# Pirx-in-OCaml: Design Notes

Companion to `PLAN.md`. This file records OCaml-specific design decisions
(ADT shapes, error monad, module layout) and breaks milestones into
session-sized chunks so we can pick work up and put it down cleanly. Add
entries as decisions land; don't retrofit after the fact.

`PLAN.md` says *what* we're porting and in what order. `DESIGN.md` says
*how* it looks in OCaml. `PROGRESS.md` says what's been done.

---

## Cross-cutting decisions

These apply to the whole port, not just M1. Revisit only if a concrete
problem surfaces.

### D1. Standard library: Base + Core (not Stdlib)

All OCaml modules in this project use Jane Street's `Base` + `Core` as
their standard library. Each `.ml` file starts with `open Core`. Core
re-exports Base, so one open covers both.

Rationale:
- Richer, more orthogonal data-structure APIs than Stdlib (consistent
  `Map`, `Set`, `Hashtbl`, `List`, `Option`, `Result`).
- Total functions by default — no silent `List.hd []` exceptions. Partial
  functions are spelled `_exn`.
- No polymorphic equality / comparison / hashing. Forces per-type
  `equal` / `compare`, which matches the Go codebase's discipline.
- `Sexp` derivers via `ppx_jane` give us AST/IR pretty-printers almost
  for free — useful for `-t ast` and `-t ir` dumps.

Consequences to be aware of while porting:
- Use `String.equal a b`, not `a = b`. `Poly.(=)` exists as an escape
  hatch; don't reach for it.
- `print_string`, `prerr_endline`, `exit`, `Printf.ksprintf` all still
  work post-`open Core` (Core re-exports them from Stdlib).
- `Stdlib.Arg` for command-line parsing is kept as-is — `Core.Command`
  is a heavier declarative framework and overkill for `pirxc`'s three
  flags. Reference it explicitly as `Stdlib.Arg.…` when needed.
- File I/O: `Out_channel.write_all` / `In_channel.read_all` from `stdio`
  (transitively in Core) replace `open_out`/`Fun.protect` patterns.
- `implicit_transitive_deps` is off in `dune-project` — each library
  declares its Core/Stdio deps explicitly. Catches layering drift.
- Preprocess directive on every library with derivers: `(preprocess (pps
  ppx_jane))`.

### D2. Error handling: exceptions for lexer/parser, accumulator for typechecker

The Go compiler uses two styles:
- Lexer and parser bail on the first error (return `error`).
- Typechecker accumulates errors in a slice and reports them all at once,
  so a user sees every type error in one run, not one-at-a-time.

We mirror this rather than invent a uniform monad:

- **Lexer/parser:** raise `exception Compile_error of Location.t * string`.
  Caught in `main.ml`, printed, `exit 1`. No error-monad plumbing in the
  hot path.
- **Typechecker:** thread a `Diag.t` (mutable error-list collector) through
  a context record. Functions return `Type.t` (defaulting to `Type.undefined`
  on error) and push diagnostics into the collector. After typechecking,
  if the collector is non-empty, print and exit.
- **IR gen / codegen:** `failwith` / `assert false`. These stages shouldn't
  produce user-facing errors — if they do, it's a compiler bug and a crash
  with a stack trace is the right UX.

Rationale: matches the Go structure, avoids boilerplate in stages that
bail anyway, keeps accumulation where it matters. A universal `result`
monad is more code for the same observable behavior. Core's `Or_error` is
available if we change our minds, but don't mix styles preemptively.

### D3. Location: plain record, by value

```ocaml
type t = { file: string; line: int; col: int } [@@deriving sexp, compare, equal]
```

Lives in its own module (`Location`). Every AST node and lexeme carries
one. No source-range tracking (start/end) — the Go version doesn't have
that either, and adding it is a separate workstream.

### D4. AST: variant types with mutable `typ` field for expressions

Expressions gain their type during type-checking. Two viable approaches:

1. **Decorate in place** — expressions carry `mutable typ: Type.t`
   initialized to `Type.undefined`, typechecker fills it in.
2. **Two AST types** — `Parsed_ast` (no types) and `Typed_ast` (with),
   typechecker returns a fresh tree.

Approach 1 is what Go does. We pick it for the port. Per PLAN.md
§Non-goals, this is not the time for a cleaner-but-different design. If
post-port we decide typed-AST-as-separate-type is worth it, that's a
dedicated refactor.

Mutable fields are ugly but contained: only the `typ` field on expression
nodes. Statement and declaration nodes are immutable. `[@@deriving sexp]`
still works fine with mutable fields.

### D5. Types: singleton `BaseType`s, structural equality via dedicated function

Mirror Go: singletons for `int`, `int8`, `int64`, `bool`, `string`,
`void`, etc. Composite types (`Pointer`, `Slice`, `Struct`) as normal
records. A function `Type.equal : t -> t -> bool` does the structural
comparison. Under Base, polymorphic `=` is disabled anyway, so the habit
is already enforced.

For M1, only `int` and `string` (and `void` internally) matter. The rest
are defined as stubs and filled in as later milestones need them.

### D6. IR Arg and Op: natural variants

Go's `Arg` is an "any-of-these-fields-is-set" struct — in OCaml a sum
type. This is one of the few places the OCaml version is cleaner than
the Go version:

```ocaml
type arg =
  | Var        of string
  | Int_lit    of int64
  | Float_lit  of float
  | String_lit of string
  | Zero
[@@deriving sexp, equal]
```

Same for `Op`: one constructor per IR op. We take this win without
considering it an "architectural change" — it's the obvious OCaml
encoding of the same data.

### D7. Module layout: one dune library per stage

Per PLAN.md §Layout, `lib/{lexer,parser,ast,types,typecheck,desugar,ir,codegen}/`
each have their own `dune` with an explicit `(libraries ...)` list. This
forces dependencies between stages to be explicit (parser depends on
lexer, typechecker on ast+types, etc.) and catches layering mistakes at
build time. Combined with `implicit_transitive_deps false` (D1) this is
strict.

Library name convention: `pirx.lexer`, `pirx.parser`, ... with public
names `Pirx_lexer`, `Pirx_parser` ... so cross-stage access looks like
`Pirx_lexer.Lexer.next`. Verbose but unambiguous.

Alternative considered: single flat library. Rejected — loses the
layering discipline and makes unit tests harder to target.

### D8. Testing framework: `alcotest`

Small, idiomatic, no ceremony. Add `test/lexer/` first; other stages
rely on the `testrunner` end-to-end suite per PLAN.md §Testing strategy.

### D9. Formatting and lint

Defer `ocamlformat` setup until post-M1 per `PROGRESS.md`. Style drift in
a small codebase isn't worth fighting yet.

### D10. CLI surface of `pirxc`

Keep the Go `pirxc` flag set verbatim (`-o`, `-O0`, `-t`). Meaningful
values for `-t` during the port:

- `ast` — print parsed AST (pre-typecheck) as S-expression.
- `final_ast` — print desugared AST.
- `ir` — print IR in the textual form Go's `Print` produces.
- `aarch64-darwin` (default) — emit `.s`.
- Other targets (`x86_64-linux`, etc.) — `die "unsupported target"`.

The `-t ast`/`-t ir` dumps are the **primary debugging tool** during the
port. Matching the Go version's textual output byte-for-byte isn't
required, but close-enough-to-diff is very valuable. With `[@@deriving
sexp]` (D1) we get most of the printer for free; a thin adapter formats
to match Go's S-expression conventions.

---

## M1 design

### M1.1. Scope recap (from PLAN.md)

Acceptance: tests 000–006 pass against OCaml `pirxc`. Features: function
decls, int vars, assignment, return, calls, string literals, `printf`,
`putchar`. No arithmetic, no control flow, no pointers, no structs.

Note: PLAN.md's aside that "003_swap uses pointers" is incorrect —
inspection shows it's plain int assignment with a temp. 003 stays in M1.

### M1.2. ADT sketches

These are the shapes we're committing to. Write actual `.mli` files from
these during the work. All modules assume `open Core` at the top.

**`Location.t`**
```ocaml
type t = { file: string; line: int; col: int } [@@deriving sexp, compare, equal]
```

**`Lexer.token`**
```ocaml
type token =
  | Tok_eof
  | Tok_ident   of string
  | Tok_number  of string     (* keep as string, parse later *)
  | Tok_string  of string     (* post-escape decoded *)
  | Tok_keyword of string
  | Tok_op      of string
  | Tok_punct   of string
[@@deriving sexp, equal]
```
Paired with `{ tok: token; loc: Location.t }`. Keeping numbers as strings
at lex time avoids deciding int vs int64 vs int8 in the lexer — matches
what Go does.

**`Type.t`** (M1 subset)
```ocaml
type t =
  | Int
  | Int8
  | Int64
  | Bool
  | String
  | Void
  | Undefined                (* for error recovery *)
  | Pointer of t             (* stubbed in M1, used by later tests *)
  | Slice   of t
[@@deriving sexp, equal]
```

**`Ast.expr`** (M1 subset)
```ocaml
type expr = {
  kind: expr_kind;
  loc:  Location.t;
  mutable typ: Type.t;   (* Undefined until typechecker runs *)
}
and expr_kind =
  | E_int_lit    of int64
  | E_string_lit of string
  | E_ident      of string
  | E_call       of { name: string; args: expr list }
[@@deriving sexp_of]
```

**`Ast.stmt`**
```ocaml
type stmt =
  | S_var_decl of { loc: Location.t; name: string; typ: Type.t option; init: expr option }
  | S_assign   of { loc: Location.t; target: expr; value: expr }
  | S_expr     of expr
  | S_return   of { loc: Location.t; value: expr option }
[@@deriving sexp_of]
```

**`Ast.func`**
```ocaml
type func = {
  loc:        Location.t;
  name:       string;
  args:       (string * Type.t) list;
  ret_type:   Type.t;
  body:       stmt list;
  external_:  bool;
}
[@@deriving sexp_of]
```

**`Ir.arg`, `Ir.op`, `Ir.func`, `Ir.program`** — as sketched in D6. For
M1, `op` has only `Assign | Call | External_call | Return | External_return`.

### M1.3. Builtin function table (M1 subset)

Hardcoded in `Typecheck.Builtins`:

| Pirx name | External name | Args | Return | Variadic |
|-----------|---------------|------|--------|----------|
| `PirxString` | `PirxString` | `len: int, ptr: *int8` | `string` | no |
| `printf` | `PirxPrintf` | `fmt: string` | `void` | yes |
| `putchar` | `putchar` | `ch: int` | `void` | no |

The full Go table (`internal/ast/functions.go`) has more entries — we
add them in later milestones as their tests require.

### M1.4. Desugaring

Only one rule for M1: a string literal `"foo"` appearing as an expression
is rewritten to `PirxString(<len>, "foo")`, where `<len>` is the byte
length (post-escape) and the `"foo"` argument passes through as a
primitive `Tok_string`-backed literal that codegen emits into `.cstring`.

The `printf` → `PirxPrintf` rename happens during IR generation by
consulting `ExternalName` on the builtin proto (mirroring Go).

### M1.5. IR synthesis

For each program, the IR generator emits:

1. One `IrFunction` per user function, with `main` renamed to `Pirx_Main`.
2. `Pirx_Init`: empty function with a single `Return()` op. Exists so
   that post-M1 global initializers have a hook. For M1 it's always
   empty but we still emit it — the codegen entry point calls it
   unconditionally.
3. A synthetic `main`: calls `Pirx_Init`, then `Pirx_Main`, then
   `ExternalReturn(result)`.

### M1.6. Codegen surface (aarch64-darwin)

The M0 hardcoded asm shows the exact template we need to hit for the
empty program. Starting from that, M1 adds:

- Local variable slots on the stack, addressed as `[sp, #offset]`.
- Temps (`$N`) also on the stack. For M1 we can use a dead-simple
  stack-slot-per-temp allocator — no register allocation. Same as Go's
  current codegen does.
- `bl _fn` for calls. Before the call: stash `x19` (frame ptr for return
  slot); after: restore.
- Variadic C-ABI on Darwin/aarch64: all `...` args go on the stack, not
  in registers. **The whole callsite sequence for `printf` depends on
  this.** Getting it wrong makes test 006 print garbage instead of
  erroring cleanly.
- String literal pool at file end:
  ```
  .section __TEXT,__cstring,cstring_literals
  .Lstr0:
      .asciz "hello world!\n"
  ```
  Label names: `.Lstr<n>` where `<n>` is a per-file counter.
- All user-visible symbols prefixed with `_` (Darwin convention).

### M1.7. Testing plan

- Lexer: alcotest unit tests in `test/lexer/`. One test per token class;
  one test for comment skipping; one for escape decoding.
- Everything downstream of lexer: tests 000–006 via `testrunner` are the
  gate. No unit tests — per PLAN.md, AST-shape unit tests are low value.
- Debug aid: `-t ast`, `-t final_ast`, `-t ir` for comparing against the
  Go compiler. Not gated, but a good sanity check at each session
  boundary.

---

## M1 work breakdown (session-sized)

Each session is a self-contained slab of work, small enough to finish in
one sitting, ending at a verifiable checkpoint. Can be done in separate
Claude Code sessions; each leaves the tree in a buildable state.

Log session completion in `PROGRESS.md` as we go.

### S1 — Library skeleton + lexer  *(target: 1 session)*

- Create `lib/{location,lexer,ast,types,typecheck,desugar,ir,codegen}/`
  with `dune` files wired for Core + `ppx_jane` (D1). Some empty
  placeholders are fine — we'll fill as we go. (Deviating slightly from
  PLAN.md by hoisting `Location` to its own module, since lexer and ast
  both depend on it.)
- `test/lexer/dune` and initial alcotest runner.
- Port `Location` module (D3).
- Port `Lexer.Token` type and `Lexer.next : Lexer.t -> lexeme`.
  - Subset per M1.2. Skip float literals, hex, `l`/`i8` suffixes,
    char literals — not needed by 000–006.
- Alcotest tests: identifiers, keywords (`func`/`var`/`return`),
  numbers, strings with escapes, operators, punctuation, comments,
  `loc` correctness.
- `bin/pirxc` still emits the M0 hardcoded asm — don't wire lexer in yet.

**Exit criteria:** `dune build && dune runtest` passes. Hardcoded-asm path
still works, `testrunner test 000` still green.

### S2 — AST + parser + `-t ast`  *(target: 1 session)*

- `Ast` module per M1.2.
- `Parser` module, recursive descent. Input: `Lexer.t`. Output: `Ast.program`.
  - Function decl.
  - Statements: var-decl (with/without init and with/without type annotation),
    assignment, expression statement, return.
  - Expressions: int lit, string lit, ident, call.
  - Errors raise `Compile_error` with the location of the offending token.
- `Ast.Pp` — S-expression printer. With `[@@deriving sexp_of]` (D1) most
  of this is free; write a thin wrapper to match Go's output format
  closely enough to diff.
- `bin/pirxc`: wire `-t ast` to parse and pretty-print. Other targets
  still hit the M0 hardcoded-asm fallback (or just `die`, since M1 is
  replacing that path).

**Exit criteria:** for each of tests 000–006, `./pirxc -t ast -o - <file>`
produces a plausible S-expression. No automated check — eyeball it (and
diff against `go run ./cmd/pirxc -t ast` if suspicious).

### S3 — Types + typechecker + desugar + `-t final_ast`  *(target: 1 session)*

- `Types` module per D5. Just `Int`, `String`, `Void`, `Undefined` needed
  for M1; stubs for the rest.
- `Typecheck.Builtins` — hardcoded table per M1.3.
- `Typecheck.Varstack` — list of `(string, Type.t)` frames for scoping.
- `Typecheck.check : Ast.program -> Ast.program`. Fills in `typ` on
  every expression. Accumulates errors per D2.
- `Desugar.run : Ast.program -> Ast.program`. Only rule: string literals →
  `PirxString` wrapping.
- `bin/pirxc`: wire `-t final_ast`.

**Exit criteria:** `./pirxc -t final_ast -o - tests/004_string_literal.pirx`
shows the `PirxString` wrapping. `./pirxc -t final_ast` on tests 000–006
produces no errors.

### S4 — IR generation + `-t ir`  *(target: 1 session)*

- `Ir` module: `arg`, `op` (M1 subset), `func`, `program`, `Pp`.
- `Ir.Generator`:
  - One `IrFunction` per source function. Rename `main` → `Pirx_Main`.
  - Emit synthesized `Pirx_Init` (empty) and `main` (wrapper) per M1.5.
  - Expression lowering: emit temps, resolve identifiers to locals,
    resolve builtin callees to `ExternalName` for `ExternalCall`.
  - No optimization.
- `bin/pirxc`: wire `-t ir`.

**Exit criteria:** for each of tests 000–006, diff `./pirxc -t ir` against
`go run ./cmd/pirxc -t ir`. Differences should be limited to temp/label
naming; op set, sizes, and argument order should match exactly. Any
structural divergence is a bug to fix before moving on.

### S5 — Codegen, non-variadic path (000, 001, 002, 003)  *(target: 1–2 sessions)*

- `Codegen.Aarch64_darwin`.
- Prologue/epilogue, stack frame.
- Stack-slot allocator for locals + temps. Simple: iterate ops once,
  assign each distinct name an offset.
- Lower ops: `Assign`, `Return`, `ExternalReturn`, `Call`, `ExternalCall`
  (non-variadic only — at most named-arg count from the builtin proto).
- Symbol prefix `_`.
- Wire the real pipeline in `bin/pirxc`: lex → parse → typecheck →
  desugar → ir-gen → codegen. Replace M0 hardcoded-asm branch entirely.

**Exit criteria:** tests 000, 001, 002, 003 pass via `testrunner`. Tests
004–006 may still fail; `testall` should show exactly 4 new passes beyond
M0's 1.

Time flex: if stack-frame math or the `main` wrapper gets hairy, split
into "S5a: Return-only (tests 000, 003)" and "S5b: Call support (001, 002)."

### S6 — Codegen variadic + string slice (004, 005, 006)  *(target: 1 session)*

- `.cstring` literal pool, `.Lstr<n>` labels.
- 16-byte struct returns from `PirxString` via `x0`/`x1`, stored back
  to the caller's slot for the slice.
- Variadic C-ABI: for the `...` portion, pass args on the stack
  (Darwin-specific). Named args still in `x0`–`x7`.
- `PirxPrintf(slice, ...)` calling convention.

**Exit criteria:** tests 004, 005, 006 pass. `testrunner testall` shows
exactly 7 tests passing (000–006) and no regressions.

### S7 — M1 wrap-up  *(target: short)*

- Update `PROGRESS.md` with M1 completion summary and any gotchas.
- Note any deviations from `PLAN.md` or `DESIGN.md` surfaced during the
  work.
- If `DESIGN.md` decisions got overruled by reality, revise them here
  and record why — future sessions should read this file and trust it.

**Exit criteria:** M1 marked ✅ in PLAN.md and PROGRESS.md.

---

## Risk register

Things likely to bite. Not action items — just things to watch.

- **Variadic ABI on Darwin/aarch64.** The whole `printf` path depends on
  getting this right. Keep `go run ./cmd/pirxc -o - tests/006_printf.pirx`
  open in another terminal for reference.
- **`PirxString` 16-byte return.** Two-register return values aren't
  exotic but they're easy to get subtly wrong (swapped halves, wrong
  store size).
- **`Pirx_Main` stack discipline.** The M0 hardcoded asm uses `x19` as
  a "return slot pointer" register — a scheme the Go codegen invented.
  Preserve it rather than redesign; compatibility with the stdlib is
  via C ABI at `_main` and extern boundaries, not internal calls.
- **Error monad creep.** If the typechecker starts wanting exceptions
  too, that's fine — document in DESIGN.md and convert. Don't let it
  drift toward "both everywhere" without deciding.
- **`Base` friction on first contact.** No polymorphic `=`, no silent
  partial functions, different `List`/`Map`/`Hashtbl` surface. Expect a
  round of small fixes in S1 while muscle memory catches up.

## Questions to resolve at milestone boundaries

Don't resolve these now; revisit when the relevant milestone is up.

- **M3:** `.err` fixture strategy (regenerate vs. substring match).
- **M5:** do we need a real type-table (interning, cycles) or can
  `Type.t` stay a plain variant? Depends on struct cross-references.
- **M9:** port the optimizer eagerly or leave `-O0` as the default for
  OCaml-built pirxc until M10?
- **M10:** delete Go compiler on swap or keep for one release?
