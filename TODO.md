# TODO

## Missing Features

### x86_64: No floating-point support

The x86_64 code generator (`internal/codegen/x86_64/`) does not support floating-point operations. All float operations (arithmetic, comparisons, negation) are only implemented in the aarch64 backend.
