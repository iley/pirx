# TODO

## Missing Features

### Float return values from external functions are miscompiled

External (C ABI) functions returning floats are silently miscompiled on both backends: `generateExternalResultStore` reads the result from the integer register (`rax`/`x0`) instead of the float register (`xmm0`/`d0`). Same for `ExternalReturn` of a float value. Fixing this requires the IR to track whether a call result / return value is a float (`ir.ExternalCall` and `ir.ExternalReturn` currently only carry the size).

### float32 is not fully supported

There is no syntax for float32 literals (the lexer never produces `ast.Literal.Float32Value`), and both code generators use double-precision instructions for all float arithmetic regardless of operand size.

### `&&` and `||` are not short-circuit evaluated

`generateBinaryOperationOps` in `internal/ir/generator.go` evaluates both operands unconditionally for `&&` and `||`. This means `false && f()` still calls `f()`. This is a correctness issue when the right operand has side effects or when the left side is a guard (e.g., `ptr != null && *ptr == 0`).
