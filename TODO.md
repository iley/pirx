# TODO

## Missing Features

### x86_64: No floating-point support

The x86_64 code generator (`internal/codegen/x86_64/`) does not support floating-point operations. All float operations (arithmetic, comparisons, negation) are only implemented in the aarch64 backend.

### `&&` and `||` are not short-circuit evaluated

`generateBinaryOperationOps` in `internal/ir/generator.go` evaluates both operands unconditionally for `&&` and `||`. This means `false && f()` still calls `f()`. This is a correctness issue when the right operand has side effects or when the left side is a guard (e.g., `ptr != null && *ptr == 0`).
