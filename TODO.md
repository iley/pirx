# TODO

## Missing Features

### Equality on strings and large structs is broken

`==`/`!=` typecheck for operands of any type, but codegen only handles register-sized values. Comparing two strings (or any 16-byte struct) panics the compiler with "invalid register size 16". Comparing structs of 8 bytes or less compiles to a word comparison, which works only by accident. Either implement proper content equality or reject non-scalar comparisons in the typechecker.

### Initializer lists are not implemented beyond the parser

The parser accepts `var p: Point = {1, 2};` but the typechecker has no case for `ast.InitializerList` and panics with "Invalid expression type: (init-list 1 2)".

### Using the result of a void function crashes or produces mangled errors

`var v = voidFunc();` panics the compiler ("unknown type %!s(<nil>)"). `1 + voidFunc()` reports "binary operation + cannot be applied to values of types int and %!s(<nil>)". The typechecker should report a clean "function has no return value" error.

### `val` constants can be mutated via `++`/`--`

`checkAssignment` rejects `=`/`+=`/`-=` on constants, but `checkPostfixOperator` and `checkPrefixOperator` don't check for constants, so `val x = 1; x++;` compiles and mutates the constant.

### `new(T, count)` silently ignores the count for non-slice types

`new(int, 5)` compiles but allocates a single int; the count expression is dropped. Should be a compile error.

### Undeclared type names panic the compiler

`var x: Undeclared;` panics with "unknown type Undeclared" in `GetSizeNoError`. The same applies to unknown types in function signatures and struct fields referenced via pointers. The typechecker should validate all type names and report a proper error (there's a TODO for this in `typechecker.go`).

### Assignment to an rvalue field panics

`f().x = 5;` (where `f` returns a struct) passes the parser's lvalue check and panics in IR generation ("unsupported expression in generateExpressionAddrOps"). Field access should only be an lvalue when its base is an lvalue.

### `printf()` with no arguments panics the compiler

Variadic functions only check declared (non-variadic) arity implicitly; calling `printf()` with zero args panics with an index-out-of-range. Calls must provide at least the declared fixed arguments.

### Duplicate definitions are not properly rejected

- Duplicate function definitions are only caught by the assembler ("symbol '_foo' is already defined").
- Duplicate struct declarations are silently accepted; the last one wins.

Both should be typechecker errors. (Duplicate globals are correctly rejected, see test 133.)

### `%` on floats passes the typechecker but fails in codegen

`binaryOperationResult` allows `%` for all numeric types, but codegen errors out with a location-less "unsupported binary operation in aarch64-darwin codegen: %". Either reject `%` on floats in the typechecker or implement fmod semantics.

### Most-negative integer literals cannot be written

`-2147483648` fails to parse because the literal is parsed as a positive number before unary minus is applied. Same for `-128i8` and `-9223372036854775808l`.

### Address-of only works on plain variables

`&s.field` and `&arr[i]` are parse errors; `parseAddressOfExpression` only accepts a variable reference. Slices have `getptr` as a workaround, struct fields have none.

### Only `+=` and `-=` compound assignments exist

`x *= 2` fails with a cryptic parse error ("unknown expression: <OPERATOR \"=\">"). Either support `*=`/`/=`/`%=` or report a better error.

### Equality and relational operators have the same precedence

`a == b < c` parses as `(a == b) < c`, unlike C where `<` binds tighter than `==`. Probably worth matching C here.

### Unknown string escapes are silently accepted

`"\q"` lexes as `q` with the backslash dropped. Should likely be a lex error.

### Float return values from external functions are miscompiled

External (C ABI) functions returning floats are silently miscompiled on both backends: `generateExternalResultStore` reads the result from the integer register (`rax`/`x0`) instead of the float register (`xmm0`/`d0`). Same for `ExternalReturn` of a float value. Fixing this requires the IR to track whether a call result / return value is a float (`ir.ExternalCall` and `ir.ExternalReturn` currently only carry the size).

### float32 is not fully supported

There is no syntax for float32 literals (the lexer never produces `ast.Literal.Float32Value`), and both code generators use double-precision instructions for all float arithmetic regardless of operand size.

### `&&` and `||` are not short-circuit evaluated

`generateBinaryOperationOps` in `internal/ir/generator.go` evaluates both operands unconditionally for `&&` and `||`. This means `false && f()` still calls `f()`. This is a correctness issue when the right operand has side effects or when the left side is a guard (e.g., `ptr != null && *ptr == 0`).
