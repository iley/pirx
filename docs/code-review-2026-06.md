# Pirx Compiler Code Review — June 2026

Full review of the compiler for 1) correctness, 2) simplicity. Six review passes
covered: lexer/parser, typechecker/desugar, IR generator/optimizer,
ast/util/drivers/stdlib, aarch64 codegen, x86_64 codegen (verified under Docker,
image `pirx-linux-x86`).

Every finding marked **CONFIRMED** was reproduced with an actual program.
Baseline at the time of review: all 156 e2e tests green at commit `8210209`.

Severity legend:

* **high** — silent wrong results, compiler crash/hang, or memory corruption on valid code
* **medium** — wrong behavior in edge cases, unsound acceptance, build failures on valid code
* **low** — diagnostics quality, tooling polish, latent hazards

---

## Part 1: Correctness findings

### Tier 1 — silent wrong results (miscompiles)

#### 1.1 Optimizer doesn't invalidate globals at calls — FIXED

`internal/ir/optimize.go:127-152`. `foldConstants` tracks `@globals` in
`knownValues` like locals and only invalidates `call.Result` at
`Call`/`ExternalCall`. A global set before a call that mutates it reads stale
afterward.

```
var calls: int;
func bump() { calls = calls + 1; }
func main(): int {
    calls = 0;
    bump();
    printf("%d\n", calls);
    return 0;
}
```

Observed: prints `0` optimized, `1` at `-O0`.

The same hole exists for pointer stores: `AssignByAddr.GetTarget()` returns
`""` (`ir.go:122-124`), so `*p = 5` invalidates nothing. The `leakedVars`
pre-scan only catches `&x` in the *current* function body; a pointer parameter
can point to a global whose address was taken elsewhere:

```
var g: int;
func f(p: *int) {
    g = 1;
    *p = 5;
    printf("%d\n", g);   // optimized: 1, -O0: 5
}
func main(): int { f(&g); return 0; }
```

Fix: invalidate all `@`-prefixed keys at every `Call`, `ExternalCall`, and
`AssignByAddr`. Locals are provably safe (a pointer to a local cannot exist
unless `&local` appears syntactically in the same function, and the pre-scan is
order-insensitive). Note: Anchor/Jump/JumpUnless already reset everything, so
this only bites in straight-line code.

**Fixed 2026-06-12**: `foldConstants` invalidates all globals at `Call`,
`ExternalCall`, and `AssignByAddr`. First unit tests added in
`internal/ir/optimize_test.go`. Regression test: `tests/157_optimizer_globals.pirx`.

#### 1.2 `g = *p` with a global target writes to stack offset 0 — FIXED

The optimizer merges `$1 = *p; @g = $1` into `UnaryOp(@g = *p)`
(`reorderAssignments` only guards ExternalCall-into-global). Both backends'
deref-store paths then look the target up in `cc.locals`, where a miss silently
yields offset 0:

* aarch64: writes to `sp+0`; prints `0` instead of `5` (confirmed natively).
* x86_64 (`x86_64_codegen.go:1192-1208, 1392-1418`): overwrites saved
  rbp/return address — prints `0` then **segfaults** (confirmed in Docker).

```
var g: int64;
func main(): int {
    var x = 5l;
    var p: *int64 = &x;
    g = *p;
    printf("%lld\n", g);
    return 0;
}
```

Only reproduces with optimization on. Two independent fixes warranted: handle
global targets in the deref-store paths, and make `cc.locals` misses panic
instead of emitting frame-corrupting stores (see simplicity item 2.2).

**Fixed 2026-06-12**: `generateMemoryCopyFromRef` in both backends now routes
global targets through their symbol (address in x2 on aarch64, RIP-relative
stores on x86_64). Additionally, all direct `cc.locals` reads go through a new
`mustLocalOffset` helper that panics on unknown names (simplicity item 2.2).
Regression test: `tests/160_global_deref_store.pirx` (int64/int/int8/struct
globals).

#### 1.3 Compound assignment double-evaluates the lvalue — FIXED

`internal/desugar/desugar.go:393-411`. `T op= V` is lowered to `T = T op V` by
desugaring the target twice; side effects in the lvalue run twice.

```
var calls: int = 0;
func f(): int { calls = calls + 1; return 0; }
func main(): int {
    var a: []int = new([]int, 3);
    a[f()] += 10;
    printf("calls=%d a0=%d\n", calls, a[0]);
    return 0;
}
```

Observed: `calls=2 a0=10` (also at `-O0`). Expected: `calls=1`. Same for
`getp().x += 10`. Inconsistent with `a[f()]++`, which the IR generator lowers
via a single address computation. Fix belongs in the IR generator (compute the
address once); expression-level desugar cannot introduce a temporary.

**Fixed 2026-06-12**: desugar keeps compound assignments intact; the IR
generator lowers them in a new `generateCompoundAssignmentOps` mirroring
`generateIncDecOps`: address once, load, binop, store through the address.
Target is still evaluated before the RHS, as the desugared form used to do.
Regression test: `tests/161_compound_assign_single_eval.pirx` (slice index,
`getp().x += v`, `*=`).

#### 1.4 Unary minus folding ignores operand size — FIXED

`internal/ir/optimize.go:289-292`. `evalUnaryOp` negates as int64 and ignores
`UnaryOp.Size`. Binary folding wraps correctly via
`performSizedIntegerArithmetic`; unary does not.

```
var m: int8 = -128i8;
var n = -m;
printf("%d\n", n);   // optimized: 128, -O0: -128
```

Same latent issue for int32 `-(-2147483648)`.

**Fixed 2026-06-12**: `evalUnaryOp` now wraps negation via
`performSizedIntegerArithmetic` using the op size. Regression test:
`tests/158_optimizer_unary_minus_overflow.pirx` plus unit tests.

#### 1.5 `util.EscapeString` corrupts string literals — CONFIRMED, high

`internal/util/strings.go:9-25`, used for all `.string` emission in both
backends. Failure modes:

* Multi-byte UTF-8 runes collapsed to one byte of the code point:
  `printf("héllo\n")` emits bytes `68 e9 6c 6c 6f` instead of `68 c3 a9 6c 6c 6f`.
* Runes > 0xFF emit `\xNNNN`, which GNU as consumes greedily, so emitted data
  is *shorter* than the compiler-computed length — `%s` over-reads adjacent
  rodata. `var s = "日本";` → `getsize(s)` says 6 but data is 2 garbage bytes.
* A `\xNN` escape followed by literal hex-digit characters merges:
  `"x\0abc"` → `.string "x\x00abc"` → as parses `\x00ab` as one escape;
  `int(s[2])` reads 0 instead of 97.
* Backslash itself is never escaped: `printf("a\\nb\n")` prints
  `a<newline>b` instead of `a\nb`.

Fix: iterate **bytes**, not runes; emit fixed-width octal escapes (`\NNN`,
exactly 3 digits, immune to the adjacency problem) for anything non-printable,
and escape `\`.

#### 1.6 aarch64 external calls: register classes starve each other — CONFIRMED, high

`internal/codegen/aarch64/aarch64_codegen.go:467-476`. The register-assignment
loop `break`s entirely when the *current* arg's register class is exhausted,
sending all remaining args to the stack — including other-class args with free
registers. Confirmed against real C callees:

* `double mix9i1d(int a..i, double x)` called with `(1×9, 0.5)` → prints
  `9.000000`, expected `9.5` (the double lands on the stack; callee reads stale d0).
* `double mix8d1i(double a..h, double i, int j)` → garbage int read.

Per AAPCS64/Apple, exhausting one class must not push other-class args to the
stack. Classification must be per-class.

#### 1.7 aarch64: named sub-8-byte stack args use 8-byte slots — CONFIRMED, high

`aarch64_codegen.go:526-563`. Every stack arg is rounded to 8 bytes, but
Apple's arm64 ABI packs *named* stack arguments at natural size and alignment
(only variadic args get 8-byte slots). C `int sum10(int a..j)` called with
1..10 returns 285 instead of 385 (10th arg read from the 9th's padding).
Variadic calls were verified correct.

#### 1.8 x86_64: float negation is a no-op when the program contains `0.0` — CONFIRMED, high

`internal/codegen/x86_64/x86_64_codegen.go:89-94, 649-656`. Negation is
`xorpd` with a pooled `-0.0` constant; the pool is a float-keyed Go map and
`-0.0 == 0.0`, so the mask dedupes to `.double 0` and the xor does nothing.

```
var z: float64 = 0.0;
var x: float64 = 3.5;
var y: float64 = -x;
printf("%.2f %.2f\n", y, z);   // observed: 3.50 0.00; expected: -3.50 0.00
```

Fix: key the literal pool on `math.Float64bits`, or emit the sign mask via a
dedicated label/inline.

#### 1.9 x86_64: rcx clobbered by global+offset loads — CONFIRMED, high

`x86_64_codegen.go:1149-1163`. The offset≠0 global-load path uses rcx as
scratch (`leaq label(%rip), %rcx; addq $off, %rcx; mov (%rcx), reg`) while
callers hold a live pointer/argument in rcx. Three confirmed corruptions:

* Global-struct = global-struct copy (16 bytes): second word written to
  *source*+16 (out-of-bounds write into the next global), destination+8 never
  written. Observed `11 0`, expected `11 22`.
* `*pp = globalStruct` (>8 bytes): same clobber of the pointer in rcx.
* Global 12/16-byte struct passed by value landing in the rcx argument slot:
  the 4th argument becomes `&global+8` (address garbage).

Fix: the formatter already supports `label+offset(%rip)` operands
(`x86_64_linux/formatter.go:66-72`); the offset path can be a single
RIP-relative mov with addend — no scratch register at all. Same fix applies to
the zero-assign global path (`:380-392`).

#### 1.10 x86_64 external-call ABI holes — CONFIRMED, high/medium

All confirmed against real C callees in Docker:

* **Int-register exhaustion sweeps remaining float args to the stack**
  (`x86_64_codegen.go:771-777`), though xmm registers are free:
  `printf("%d %d %d %d %d %d %f %f\n", 1..6, 7.5, 8.5)` prints
  `... 8.500000 0.000000`. Per SysV, exhaustion is per-class. (high)
* **Stack-passed floats use xmm0 as scratch**, clobbering the first float
  argument (`:849-851`): printf with 9 floats prints the first as `9.0`.
  Scratch must be a non-argument register (xmm8-15) or go through rax. (high)
* **Odd-size stack args don't advance to the next eightbyte** (`:873-897`):
  a 12-byte struct followed by an int → the callee reads 0 instead of 70.
  Missing `stackOffset = util.Align(stackOffset, 8)` after each copy. (medium)
* **Structs with float members are passed/returned in int registers**
  (`:758-818, 246-303, 966-1003`): SysV requires SSE-class eightbytes;
  C callee receiving `{1.25, 2.5}` sees `0.0 0.0`. Return direction is equally
  wrong (works only by accident with gcc -O0 callees). Either implement
  eightbyte classification or reject float-member structs in extern
  signatures. (medium — breaks genuine C interop only)

Note: a single trailing float after register exhaustion *accidentally* prints
correctly (two bugs canceling), so tests need ≥2 floats to catch this.

#### 1.11 `func main()` without return type exits with garbage status — FIXED

`internal/ir/generator.go:849` (`generateMain`). The entry stub always emits
`Call4($ret = Pirx_Main()); ExternalReturn4($ret)`; for a void `main`, `$ret`
is an uninitialized stack slot. `func main() { printf("x\n"); }` exits with
status 200 on aarch64-darwin, 255 on x86_64-linux. All 156 e2e tests use
`main(): int` with explicit `return 0`, which masks this. Fix: zero-initialize
`$ret` (one IR op), or only thread a result when main is declared `: int`.
Affects all backends.

**Fixed 2026-06-12**: `generateMain` only threads `$ret` when main is declared
with a return type; for void main it returns a literal 0. Regression test:
`tests/156_void_main.pirx`.

### Tier 2 — compiler crashes / hangs on valid code

#### 2.1 `f().x` panics the compiler — CONFIRMED, high

`internal/ir/generator.go:451` (via `:454-474`). Every field read is lowered
through `generateExpressionAddrOps`, which only handles
VariableReference / `*p` / FieldAccess / IndexExpression. Any rvalue struct
(function-call result) hits the panic — no `&` needed in source:

```
struct Point { x: int; y: int; }
func mk(): Point { var p: Point; p.x = 7; return p; }
func main(): int {
    var a = mk().x;        // panic: unsupported expression in generateExpressionAddrOps
    printf("%d\n", a);
    return 0;
}
```

Fix: in `generateFieldAccessAddrOps`'s non-pointer branch, spill the rvalue to
a fresh temp and take the temp's address.

Related: `&mk().x` reaches the same panic because the typechecker accepts `&`
on anything — `unaryOperationResult("&", ...)` unconditionally succeeds
(`typechecker.go:916-919`) while a correct `isAddressable` sits unused at
`typechecker.go:527`. Fix: require `isAddressable` for `&` operands.

#### 2.2 Constant-folded division by zero panics the compiler — FIXED

`internal/ir/optimize.go:185, 201, 217`. `performSizedIntegerArithmetic`
executes the Go division; a constant 0 divisor crashes the compiler:

```
var zero = 0;
var x = 10 / zero;    // optimized: compiler panic; -O0: builds, prints 0 (sdiv semantics)
```

Reachable via propagation, so the typechecker cannot fully guard it. Fix:
don't fold `/` when the RHS folds to 0. (`%` is currently safe only because it
isn't in the fold list; `MinInt / -1` is fine — Go wraps per spec, matching
hardware.)

**Fixed 2026-06-12**: `evalBinaryOp` refuses to fold integer `/` when the RHS
folds to 0, leaving the division for runtime. Regression test:
`tests/159_optimizer_div_by_zero_fold.pirx` plus unit tests.

#### 2.3 `resize` on a zero-cap slice hangs forever — CONFIRMED, high

`stdlib/builtin.c:25-29`. `new_cap *= 2` never escapes 0:

```
var s: []int;
resize(&s, 10);   // hangs forever
```

Fix: `if (new_cap == 0) new_cap = 1;` (or start from the requested size).
Related (medium): `realloc` growth is not zeroed (`builtin.c:31`), unlike
`new` which uses calloc — grown elements expose stale heap data; `memset` from
old cap to new.

#### 2.4 aarch64 emits unencodable assembly for large frames/arg areas — CONFIRMED, medium

Build failures (assembler errors), not silent corruption. One immediate-range
family:

* Frames > 4095 bytes: `sub/add sp, sp, #imm` exceeds the 12-bit immediate
  (`aarch64_codegen.go:195-202`); same for `add reg, sp, #imm` address loads
  (`:947-948`) and struct copies through pointers (`:1003-1004, 1025-1026`).
* Native-call argument staging writes at `[sp, #-N]` *before* `sub sp`
  (`:397-438`); beyond ±256 only stur/ldur encode, and the x19 reload (`:434`)
  has no range handling. Repro: 320-byte struct passed by value.
* External calls with > 256 bytes of stack args, same pattern (`:548-560`).
  Repro: printf with 33 int varargs.
* `MAX_SP_OFFSET_X = 504` ignores that 8-byte scaled offsets in 256–504 must
  be a multiple of 8 (`:859-886, 1196-1226`). Repro: `func f(b: bool, x: int64)`
  with ~240 bytes of locals → `ldr x0, [sp, #308]` rejected.
* A 12-byte struct as a *stack* argument panics the compiler:
  `registerByIndex(10, 12)` → `panic: invalid register size 12` (`:554` →
  `:1161`). Register-passed 12-byte structs work.

Suggested fix that kills most of the class: `sub sp` first, then store at
positive offsets (also removes the latent below-SP signal-clobber hazard —
macOS does not guarantee a red zone); add encodability checks
(`off <= 255 || (off%8 == 0 && off <= 504)`) with an x9 fallback; split large
SP adjustments.

#### 2.5 IR generator swallows all of its own errors — CONFIRMED, medium

`internal/ir/generator.go:60` ends with `return irp, nil`; every `errorf`
diagnostic (`:902-904`) is appended to `g.errors` and discarded.
`cmd/pirxc/main.go:136-142` already has handling code that never fires.
Downstream effect: error paths continue with nil/garbage — e.g. `getField`
miss returns nil which the caller dereferences (`field.Size` at `:459`), a
nil-pointer panic instead of a diagnostic. Fix: `return irp, g.errors`.

### Tier 3 — unsound typechecking / wrong rejection

#### 3.1 `new([]T, count)` never checks the count type — CONFIRMED, high

`internal/typechecker/typechecker.go:768-789`. `new([]int, true)` compiles and
runs (bool reinterpreted as count); `new([]int, "hello")` panics codegen
(`aarch64_codegen.go:930`). Fix: require an integer type for the count.

#### 3.2 `val` const-ness only enforced for direct assignment — CONFIRMED, medium

`typechecker.go:503-510, 829-838`. All of these compile and mutate constants:
`*&c = 7`, `var p: *int = &c; *p = 9`, and for `val p: Point`:
`p.x = 99; p.y++;`. At minimum, `&const` and `constVar.field = ...` deserve
checks (extend the const lookup to FieldAccess bases and `&` operands).

#### 3.3 Non-interned `file` type breaks `==` — CONFIRMED, medium

`typechecker.go:993` uses pointer identity (`typ == ast.File`) but
`NewBaseType` (`ast/types.go:60-81`) does not intern `"file"`. Result:
`var f: file = open(...); f == g` is rejected while the identical program with
inferred types compiles. Fix: intern `"file"` (one line) or compare by name.
See also simplicity item 2.6 — the identity-vs-name dual equality is a
standing fragility class.

#### 3.4 Duplicate struct field names accepted silently — CONFIRMED, medium

`internal/ast/typetable.go:179-207`. `struct P { x: int; x: int; }` compiles;
`GetField` returns the first match. Fix: a seen-set in
`makeStructDescriptor`.

#### 3.5 `while (true) { return 1; }` rejected for missing return — CONFIRMED, low

`typechecker.go:931-954`. `statementReturns` has no case for while/for. Even
the conservative rule (a `while(true)` with no `break` only exits via return)
would accept this.

#### 3.6 `getptr(string)` is unusable — CONFIRMED, medium

`typechecker.go:383-399`. `getptr`'s `AnySlice` parameter accepts strings, but
`resolveSpecialFunctionReturnType` only special-cases slices, so for a string
it returns the proto's `voidptr` — not assignable and not inferable. `range`
handles strings correctly right next to it; `getptr` should return `*int8`.

#### 3.7 Non-ASCII char literals silently truncate — CONFIRMED, medium

`internal/parser/parser.go:1067`. `int8([]rune(lex.Str)[0])` wraps any
codepoint > 127: `'é'` becomes -23 with no diagnostic. Fix: reject char
literals outside int8 range.

#### 3.8 Generated `PirxEq_<Struct>` collides with user functions — CONFIRMED, medium

`internal/desugar/desugar.go:445-455`. Defining `func PirxEq_Point(...)` and
also using `==` on `Point` values → assembler error `symbol '_PirxEq_Point'
is already defined` leaks to the user. Fix: reserve the prefix with a
diagnostic, or uniquify.

#### 3.9 Struct declaration order matters — CONFIRMED, low

`ast/typetable.go:165-174`. `struct A { b: B; } struct B { x: int; }` →
"type B is not fully defined". Acknowledged in a code TODO but not in TODO.md.

### Tier 4 — runtime / stdlib

#### 4.1 `%s` ignores Pirx string length — CONFIRMED, medium

`stdlib/builtin.c:82-105`. Pirx strings are length+data; substrings from
`range` are not NUL-terminated at `size`:

```
var sub = range("hello world", 0, 5);
printf("sub=[%s] len=%d\n", sub, getsize(sub));   // sub=[hello world] len=5
```

Same no-NUL assumption breaks `cstr()` and `open()` (fopen on `data`) for any
computed/substring string. PirxPrintf alone can't fix it (varargs lose the
size); needs codegen+stdlib coordination (e.g. pass size and rewrite `%s` →
`%.*s`).

#### 4.2 `open()` failure is undetectable; `readline` on it segfaults — CONFIRMED, medium

`stdlib/builtin.c:107-117`. `PirxOpen` returns NULL on failure; `file == null`
is a typechecker error; `PirxReadLine` calls getline on the NULL handle →
SIGSEGV. Minimal fix: allow file/null comparison or make PirxReadLine return
empty on NULL.

#### 4.3 No slice bounds checking anywhere — CONFIRMED, medium (design gap)

`s[100000000]` on a 3-element slice silently returns 0; `s[3]` write succeeds
(heap corruption); `range(s, 2, 10)` produces an out-of-bounds view. `Panic()`
is declared in `builtin.h:6` but defined and called nowhere. If intentional,
document in TODO.md; otherwise the `Panic` hook is the place to start.

#### 4.4 Minor runtime nits — SUSPECTED, low

* `PirxAlloc(elem_size * cap)`: int32 overflow for huge slices → negative size
  → NULL calloc, unchecked → segfault later. No malloc-failure handling
  anywhere.
* `PirxString` claims `cap = len + 1` over rodata it doesn't own — observable
  via `getcap`; harmless today (resize on strings is rejected) but a trap.
* `PirxReadLine` returns size 0 for both EOF and empty line; distinguishable
  only via `getptr(line) == null`. API wart worth a TODO.md entry.

### Tier 5 — tooling / drivers

#### 5.1 `pirx build` clobbers and deletes user files — CONFIRMED, medium

`cmd/pirx/main.go:194-257`. Intermediates `<base>.s`/`<base>.o` are placed
next to the output and unconditionally removed:

* A pre-existing user `foo.s`/`foo.o` next to the output is overwritten, then
  deleted.
* `pirx build foo.pirx -o foo.s` reports success and then *deletes the output*
  (asmFile == binFile).
* `foo.pirx` + `foo.c` inputs collide on `foo.o`; the C compile overwrites the
  Pirx object → "Undefined symbols: _main".
* Failed builds leave a 0-byte `.s` behind.

Fix for the whole family: put intermediates in `os.MkdirTemp` (also deletes
the `-k` cleanup bookkeeping).

#### 5.2 `pirxc -t ast` ignores `-o` and truncates the output file — CONFIRMED, low

`cmd/pirxc/main.go:63-107, 130`. `-t ast`/`-t final_ast` print to stdout while
`-t ir` honors `-o`; worse, the output file is created (truncated) before the
early return, so `pirxc -t ast foo.pirx` leaves an **empty foo.s**, destroying
a previous good artifact. Move `os.Create` after the ast branches and write to
`output`.

#### 5.3 testrunner has no timeout on test programs — CONFIRMED, low

`cmd/testrunner/main.go:744-757`. Plain `CombinedOutput()`; any hanging test
(e.g. finding 2.3) wedges `make test` indefinitely. Fix:
`exec.CommandContext` with a timeout. (Comparison logic is otherwise sound:
exact equality, nonzero exit fails, error tests that compile fail.)

#### 5.4 Non-deterministic assembly output — CONFIRMED, low

`aarch64_codegen.go:77-91` (same pattern in x86_64): string/float literal
emission iterates maps in random order; 5 compiles of one file produced 2
distinct .s files. Semantically correct but not reproducible. Fix: iterate
sorted keys.

#### 5.5 Diagnostics polish — CONFIRMED, low

* EOF lexeme has zero-value `Loc` (`lexer.go:240`), so parser errors at EOF
  report `:0:0`; `parseBlock`'s EOF check emits `unexpected EOF` with no
  location at all (`parser.go:443`).
* Unterminated string/char literals return bare `io.ErrUnexpectedEOF` with no
  position (`lexer.go:485, 538, 554, 586`).
* `0x` with no digits is lexed as a NUMBER; parser fails with an
  internals-leaking strconv message (`lexer.go:621-624`, `parser.go:987-998`).
* N consecutive `//` comments = N recursive `Next()` frames (`lexer.go:277`);
  5M comment lines → stack overflow. Make it a loop.
* Raw newlines silently accepted in string literals (`lexer.go:481-529`) —
  possibly intended, undocumented; makes missing-quote errors swallow the file.
* No float exponent syntax: `1.5e10` lexes as `1.5` + ident `e10` with a
  misleading error; `123abc` splits silently. A "letter may not follow a
  number" check fixes the diagnostics.
* No block comments; `/* */` produces a confusing error. Not in TODO.md.
* Two typechecker messages are missing the `:` after the location
  (`typechecker.go:444, 448`); the AnySlice branch omits the function name
  (`:440`).
* `pirx build -O<x>` for x≠0 forwards a flag pirxc doesn't define → confusing
  flag-parse error.
* testrunner registers a test twice if both `.out` and `.err` exist; merges
  stdout/stderr via CombinedOutput (nondeterministic interleaving).

#### 5.6 Latent hazards — SUSPECTED, low

* x86_64: AL not zeroed for variadic calls using no xmm registers
  (`x86_64_codegen.go:902-906`); SysV requires AL ≤ 8 as a vector-register
  count. Benign with gcc/glibc/musl prologues, formally UB. One-instruction fix.
* x86_64: native-call args stored below rsp before `subq` (`:709-718`); beyond
  the 128-byte red zone these can be clobbered by signal delivery.
  Unobservable today; `subq` first costs nothing.
* Struct field alignment uses field *size* with a power-of-two-only
  `util.Align` (`typetable.go:184-204`, `bitops.go:7-9`). Sizes like 12/24
  happen to produce correctly-aligned offsets (verified at runtime), but it
  works by accident and over-pads. Compute real per-type alignment.
* `int8` returns from Pirx-bodied extern funcs fail with "unsupported return
  value size: 1" on both backends. Worth a TODO.md entry.
* IR generator FIXME at `generator.go:499` ("condSize is 8 when it should be
  4???") appears obsolete — all observed branch condition sizes are 4.

---

## Part 2: Simplicity findings

The compiler is structurally sound, but three systemic issues stand out:

1. **Heavy copy-paste within and between the two backends.** The same mistakes
   were made independently in both (findings 1.6/1.7 vs 1.10 are the same bug
   twice).
2. **Zero unit tests in exactly the packages where bugs cluster** — internal/ir,
   internal/typechecker, internal/desugar have no test files at all, and
   internal/parser has none (only the lexer is unit-tested). All coverage is
   end-to-end.
3. **Two equality semantics for types** (pointer identity of singletons vs
   `Equals` by name) — already produced real bug 3.3.

Refactoring ideas, roughly by return-on-effort:

### 2.1 Chunked-copy helper in each backend — small

aarch64 writes the 8/4/1 chunked-copy loop five times
(`aarch64_codegen.go:351-363, 797-825, 1040-1059, 1063-1082, 1086-1111`);
x86_64 pastes the `movb/movl/movq` size-switch ~8 times despite having
`sizeToSuffix` (`x86_64_codegen.go:1094-1106, 1117-1132, 1134-1167, 1169-1208,
1420-1435`). Extract a `forEachChunk(size)` helper per backend: removes ~100
lines and gives one place to fix offset-encoding bugs (2.4).

### 2.2 `mustLocalOffset` that panics on unknown names — DONE

Both backends silently map `cc.locals` misses to offset 0 — the root of the
frame-smashing bug 1.2. A lookup helper that panics on unknown/global names
converts silent memory corruption into a compile-time failure permanently.

**Done 2026-06-12** alongside the fix for 1.2.

### 2.3 Optimizer deduplication — small

* `Call`/`ExternalCall` branches in `foldConstants` are verbatim copies
  (`optimize.go:127-152`); extracting the arg-substitution helper is also the
  natural single place for the fix to 1.1.
* `performSizedIntegerArithmetic` (`:174-226`) is three copies of one switch.
  Compute in int64, truncate to width once — also where the 1.4 fix lands.
* The float-folding code (`performFloatArithmetic`, `isFloatArg`, `:228-261`)
  is **dead**: the generator emits float ops as `+.`/`*.`/… which
  `evalBinaryOp` never matches. Delete it, or add the dotted ops (making it
  live).
* The leaked-vars scan is implemented twice (`:71-77` inline vs
  `findLeakedVars` `:465-475`).
* `renameVariables` (`:579-634`) mutates the input ops' `Args` backing arrays
  in place — aliasing footgun; clone before mutating.
* `Optimize` (`:19-26`) copies `IrFunction` field-by-field; any future field is
  silently dropped.

### 2.4 Shared register classifier for aarch64 extern prologue + call — medium (~2h)

`generateExternalPrologue` (`aarch64_codegen.go:237-293`) and
`generateExternalFunctionCall` (`:443-521`) duplicate the size-1/4/8/12/16,
int-vs-float classification switch. One shared classifier fixes bugs 1.6 and
the 12-byte-stack-arg panic once instead of twice.

### 2.5 Type switches instead of assertion chains — small, mechanical

* `checkStatement`/`checkExpression` (`typechecker.go:189-247`),
  `generateStatementOps`/`generateExpressionOps` (`generator.go:128-187,
  237-263`), `ReplaceTarget` (`ir.go:377-409`) — convert if-else type-assert
  chains to `switch x := x.(type)`.
* `checkPostfixOperator`/`checkPrefixOperator` (`typechecker.go:791-824`) are
  line-for-line duplicates; extract a shared helper.
* Use `ast.Undefined` (not `ast.Int`) as the error-recovery placeholder
  (`typechecker.go:597, 616, 746, 752, 757, 762`) to stop misleading cascade
  errors.
* `desugarStatement`'s silent `default: return stmt` (`desugar.go:107-109`)
  should enumerate cases and panic on unknown statement kinds (the expression
  counterpart already does).
* The pseudo-type argument dispatch (`typechecker.go:427-453`) is a 6-branch
  if-else over sentinel types; a table of `pseudoType → (predicate,
  description)` collapses it and fixes the inconsistent message formats (5.5)
  in one place.

### 2.6 Intern all base types / unify type equality — small

`IsIntegerType`/`IsFloatingPointType` (`ast/types.go:136-146`) use pointer
identity while `Equals` (`:89-94`) compares names. Route all type construction
through interning (add `"file"` and any future singletons to `NewBaseType`) or
switch the predicates to name comparison. Removes the entire class behind 3.3.

### 2.7 Lexer table-driving — small/medium

* ~8 near-identical two-char-operator blocks in `Next()`
  (`lexer.go:245-445`, ~180 lines): a `map[rune][]{second, token}` table plus
  one helper cuts ~120 lines.
* Escape-sequence switch duplicated between lexString and lexChar
  (`:507-524` vs `:560-577`): extract `decodeEscape`.
* `l`/`i8` suffix handling duplicated between lexNumber and lexHexNumber
  (`:664-686` vs `:714-734`).
* `str += string(r)` accumulation is O(n²); use `strings.Builder`.

### 2.8 Parser cleanups — small

* The `val`/`var` top-level branches in `Parse` (`parser.go:81-108`) are
  character-identical; merge.
* Postfix `++`/`--` branches (`:853-888`) identical except the operator string.
* `parseIdentifierExpression` (`:1309-1336`): the lookahead fast path is
  redundant — the save/consume/peek/restore path works unconditionally.
* `locationFromLexeme` (`:12-14`) is an identity accessor; inline it.
* `statementRequiresSemicolon` (`:477-496`): four sequential type asserts →
  one multi-type switch case.

### 2.9 Driver / testrunner cleanups — medium

* `cmd/pirx`: argument classification is ~70 lines for "one dir or N files";
  a single loop with a "directory only as sole argument" check covers it.
  `buildProgram` takes 8 parameters; a temp dir for intermediates (fixes 5.1)
  removes `keepIntermediate`, `isDirectoryBuild`, and `buildDir` plumbing.
* `cmd/testrunner`: `runSingleTest`/`runSingleTestQuiet` are byte-identical
  except one printf (`main.go:382-479`); `compileTest`/`compileProgram` are
  near-duplicates (`:658-742`); `findPirxFile` could reuse `findTestCase`
  (`:553-641`); `readExpectedOutput`/`readExpectedError` are identical
  wrappers.
* `.err` reference files bake in driver noise (`pirxc compilation failed: exit
  status 1`, cobra's `Error: ...`); any driver-message change churns every
  error test. Consider printing only pirxc's stderr.

### 2.10 Formatter and backend unification — medium/large

* aarch64 darwin vs linux formatters are ~85% identical (label prefix, page
  operators, immediate syntax, comment char, sections). A single formatter
  parameterized by a small syntax struct halves the code. Medium (~2h).
* The two backends mirror each other nearly function-for-function (~2700 lines
  combined): same dispatch, frame layout, copy loops, literal pools, extern
  marshalling — independently maintained and independently buggy. A shared
  skeleton in `codegen/common` (frame/locals assignment, op dispatch, copy
  plans) with per-arch instruction emitters would prevent divergence. Large
  (multi-day); worthwhile mainly if more targets are planned — the per-file
  deduplication above is the better near-term return.

### 2.11 Dead code — trivial

* `util.Align64` (`bitops.go:3-5`) — no callers.
* `util` pointer helpers `Int32Ptr`/`Int64Ptr`/`BoolPtr` → one generic
  `Ptr[T]`; note `Int32Ptr(value int)` silently truncates.
* aarch64: commented-out `fPRegisterByIndex` (`:1165-1175`); `asm.Arg.WithLSL`
  (`asm.go:53-57`); `common.NameAndSize` (`gather.go:44-47`).
* x86_64: `FUNC_CALL_REGISTERS` constant (`:18`) unused (literal `argRegisters`
  slices used instead, twice — hoist to one var); `registerByIndex` index 1
  never used; second return of `get8And32BitRegNames` ignored at its only call
  site.

---

## Part 3: Suggested fix order

Each fix has a ready-made repro that can become a `tests/NNN_*.pirx` case.

1. ~~**1.11** void-main exit status (one line in `generateMain`).~~ DONE
2. **1.1 + 1.4 + 2.2** optimizer soundness (one small change surface in
   optimize.go; add the first internal/ir unit tests here).
3. **1.3** compound-assignment double-eval (lower in IR generator).
4. **2.1** `f().x` spill + use `isAddressable` for `&` in the typechecker.
5. **1.5** EscapeString byte-wise octal escaping.
6. **2.3** resize cap-0 hang + zero grown memory; add testrunner timeout (5.3).
7. **3.1** new() count type check.
8. aarch64 ABI cluster (**1.6, 1.7, 2.4**) via the shared classifier (2.4
   refactor).
9. x86_64 cluster (**1.8, 1.9, 1.10**) — start with the rcx fix
   (RIP-relative + addend) and the literal-pool bit-pattern key.
10. Remaining Tier 3/4/5 items opportunistically.

TODO.md candidates (missing features discovered, per project policy):
no slice bounds checking, no block comments, struct declaration order matters,
`int8` returns from Pirx-bodied extern funcs unsupported, `file`/`null`
comparison rejected, `readline` cannot distinguish EOF from empty line,
no float exponent literal syntax.

---

## Appendix: verified-correct areas

To save future review effort, these were explicitly exercised and found
correct:

* **Parser/lexer**: full C-style precedence/associativity battery (by computed
  values); int32/int64/int8 literal boundaries including most-negative values
  and out-of-range rejection; hex literals incl. `0X`; prefix/postfix `++`/`--`;
  member/index/call chaining; dangling else; assignment-target and address-of
  syntactic validation; error column accuracy; stray bytes / invalid UTF-8
  produce clean errors.
* **Typechecker/desugar**: short-circuit `&&`/`||` incl. nested forms (RHS not
  evaluated when short-circuited); for-loop `continue` rewriting; struct
  equality (nested, string fields by content, empty structs, `!=`); duplicate
  functions/args/vars; use-before-declaration; break/continue outside loops;
  initializer-list arity/type errors.
* **IR/optimizer**: knownValues reset at every label/jump (loops safe); folding
  wraparound for `+ - *` at all widths matches runtime; left-to-right
  evaluation order; `removeIneffectiveAssignments` protects address-taken vars
  and globals; `coalesceVariables` conservative and sound.
* **aarch64**: signed div/mod incl. negatives; int8 sign-extension/wraparound/
  promotion; 64-bit literal materialization; float arithmetic and all six
  comparisons with correct NaN semantics; variadic calls per Apple ABI (mixed
  12-arg printf); 12/16-byte struct passing/returning both directions vs real
  C; recursion; x19 discipline; globals incl. initializer lists; medium frames
  (>504 offsets via x9).
* **x86_64**: movabs for 64-bit immediates; int8 division (movsbl + cltd/idivl);
  float comparison NaN handling (ucomisd + setnp/setp, all six ops);
  16-byte call-site alignment; rbx/rbp save discipline; RIP-relative
  addressing (PIE-safe); bool widening at extern boundaries.
* **Runtime**: struct layout/strides at runtime; `PirxStringEq` incl.
  empty-string NULL-data guard; `getsize`; `readline` newline handling.

Review conducted 2026-06-11/12 at commit `8210209` (clean tree). Repro
programs were written under /tmp (not preserved in the repo).
