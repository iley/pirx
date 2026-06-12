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

## Bugs

### Assigning to a string element compiles but can crash

`s[0] = 'x'` typechecks (string indexing is addressable like slice indexing), but string literal data lives in read-only memory, so writing to a literal-backed string segfaults at runtime. Strings are otherwise treated as immutable values (compared by content, `range` copies); index assignment on strings should probably be a typechecker error.
