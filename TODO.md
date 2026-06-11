# TODO

## Missing Features

### Initializer lists only work in variable declarations

`var p: Point = {1, 2};` works (including nested structs and globals), but initializer lists are not allowed in other contexts such as assignments, function arguments or return values. They also only work for struct types, not slices.

### Equality on slices is rejected rather than implemented

`==`/`!=` work on scalar values, strings (by content) and structs (field by field), but comparing slices, or structs containing slice fields, is a typechecker error.

### `%` on floats is rejected rather than implemented

`%` only works on integers. Implementing fmod semantics for floats is an option.

### External functions with bodies only accept register arguments

An `extern func` *defined in Pirx* receives its parameters via a C-ABI prologue that spills the incoming registers into the function's stack slots. Arguments that the C ABI passes on the stack (i.e. beyond 8 integer/float registers on aarch64, 6 integer/8 float registers on x86_64) are not supported and cause a compile-time error.

### float32 is not fully supported

There is no syntax for float32 literals (the lexer never produces `ast.Literal.Float32Value`), and both code generators use double-precision instructions for all float arithmetic regardless of operand size.
