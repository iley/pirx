# TODO

## Missing Features

### Initializer lists only work in variable declarations

NOTE: This is not planned for now.

`var p: Point = {1, 2};` works (including nested structs and globals), but initializer lists are not allowed in other contexts such as assignments, function arguments or return values. They also only work for struct types, not slices.

### Equality on slices is rejected rather than implemented

`==`/`!=` work on scalar values, strings (by content) and structs (field by field), but comparing slices, or structs containing slice fields, is a typechecker error.

### `%` on floats is rejected rather than implemented

`%` only works on integers. Implementing fmod semantics for floats is an option.

### Structs with floating-point fields can't cross the extern boundary

The C ABI passes such structs in float registers (SSE-class eightbytes on x86_64, HFAs on arm64), which the code generators don't implement — they only track per-argument int/float, not per-eightbyte classes. Rather than silently passing garbage, the typechecker rejects structs with float fields (including nested ones) in extern function signatures, return types and variadic arguments. Implementing this properly requires per-eightbyte class information in the IR plus SSE/HFA marshalling in both backends, for arguments and return values alike.

### External functions with bodies only accept register arguments

An `extern func` *defined in Pirx* receives its parameters via a C-ABI prologue that spills the incoming registers into the function's stack slots. Arguments that the C ABI passes on the stack (i.e. beyond 8 integer/float registers on aarch64, 6 integer/8 float registers on x86_64) are not supported and cause a compile-time error.

### float32 is not fully supported

There is no syntax for float32 literals (the lexer never produces `ast.Literal.Float32Value`), and both code generators use double-precision instructions for all float arithmetic regardless of operand size.

### No slice bounds checking

Slice reads and writes are not bounds-checked: `s[100]` on a 3-element slice silently reads garbage, an out-of-bounds write corrupts the heap, and `range(s, 2, 10)` produces an out-of-bounds view. The unused `Panic()` hook declared in `stdlib/builtin.h` is the natural place to start if checks are added. Needs a design decision (runtime cost vs. safety, behavior on violation).

### No block comments

Only `//` line comments exist; `/* */` produces a confusing parse error.

### Struct declaration order matters

`struct A { b: B; }` fails with "type B is not fully defined" when `B` is declared after `A`. Forward references between struct types are not resolved.

### int8 returns from Pirx-bodied extern funcs unsupported

An `extern func` defined in Pirx with an `int8` return type fails with "unsupported return value size: 1" on both backends.

### readline cannot distinguish EOF from empty line

`readline` returns a size-0 string both at EOF and for an empty line; the only way to tell them apart is `getptr(line) == null`.

### No float exponent literal syntax

`1.5e10` lexes as `1.5` followed by an identifier `e10`, producing a misleading error.

## Bugs

### Assigning to a string element compiles but can crash

`s[0] = 'x'` typechecks (string indexing is addressable like slice indexing), but string literal data lives in read-only memory, so writing to a literal-backed string segfaults at runtime. Strings are otherwise treated as immutable values (compared by content, `range` copies); index assignment on strings should probably be a typechecker error.
