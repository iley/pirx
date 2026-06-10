# TODO

## Bugs

### printf reads garbage for any argument after a %s

`printf("%s %lld\n", "hi", 99l)` prints `hi 12884901890` instead of `hi 99`. A string argument is passed to `PirxPrintf` as a full 16-byte slice (data pointer, size, cap), but `PirxPrintf` forwards the varargs straight to C's `vprintf`, whose `%s` consumes only the 8-byte data pointer. All subsequent arguments are then read 8 bytes off (the first one gets the string's size/cap words). Fix options: make `PirxPrintf` interpret the format string itself, or pass strings as plain `char*` and the length separately.

## Missing Features

### Initializer lists only work in variable declarations

`var p: Point = {1, 2};` works (including nested structs and globals), but initializer lists are not allowed in other contexts such as assignments, function arguments or return values. They also only work for struct types, not slices.

### Equality on strings and structs is rejected rather than implemented

`==`/`!=` only work on scalar values (numbers, booleans, pointers, files); comparing strings, structs or slices is a typechecker error. Content equality could be implemented instead.

### `%` on floats is rejected rather than implemented

`%` only works on integers. Implementing fmod semantics for floats is an option.

### External functions with bodies only accept register arguments

An `extern func` *defined in Pirx* receives its parameters via a C-ABI prologue that spills the incoming registers into the function's stack slots. Arguments that the C ABI passes on the stack (i.e. beyond 8 integer/float registers on aarch64, 6 integer/8 float registers on x86_64) are not supported and cause a compile-time error.

### Address-of only works on plain variables

`&s.field` and `&arr[i]` are parse errors; `parseAddressOfExpression` only accepts a variable reference. Slices have `getptr` as a workaround, struct fields have none.

### float32 is not fully supported

There is no syntax for float32 literals (the lexer never produces `ast.Literal.Float32Value`), and both code generators use double-precision instructions for all float arithmetic regardless of operand size.
