# TODO

## Missing Features

### Initializer lists are not implemented beyond the parser

The parser accepts `var p: Point = {1, 2};` and the typechecker reports a proper "initializer lists are not supported" error. The actual feature (typechecking and IR generation) is not implemented.

### Equality on strings and structs is rejected rather than implemented

`==`/`!=` only work on scalar values (numbers, booleans, pointers, files); comparing strings, structs or slices is a typechecker error. Content equality could be implemented instead.

### `%` on floats is rejected rather than implemented

`%` only works on integers. Implementing fmod semantics for floats is an option.

### External functions with bodies don't receive their parameters

An `extern func` *defined in Pirx* compiles, but the function reads its parameters from the Pirx-internal stack locations while callers pass them in registers per the C ABI, so the parameters contain garbage. Affects both integer and float parameters on both backends. Fixing this requires a C-ABI prologue that spills the incoming register arguments into the function's stack slots (the IR also doesn't track which args are floats, see `ir.IrFunction`).

### Address-of only works on plain variables

`&s.field` and `&arr[i]` are parse errors; `parseAddressOfExpression` only accepts a variable reference. Slices have `getptr` as a workaround, struct fields have none.

### float32 is not fully supported

There is no syntax for float32 literals (the lexer never produces `ast.Literal.Float32Value`), and both code generators use double-precision instructions for all float arithmetic regardless of operand size.
