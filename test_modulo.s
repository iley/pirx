.globl _main
.p2align 2
_main:
  sub sp, sp, #16
  stp x29, x30, [sp]
  mov x29, sp
  sub sp, sp, #16
// Op 0: BinaryOp($1 = 10 % 3)
  mov x0, #10
  mov x1, #3
  sdiv x2, x0, x1
  msub x0, x2, x1, x0
  str x0, [sp, #0]
// Op 1: Return($1)
  ldr x0, [sp, #0]
  b .Lmain_exit
.Lmain_exit:
  add sp, sp, #16
  ldp x29, x30, [sp]
  add sp, sp, #16
  ret

.globl _test_modulo
.p2align 2
_test_modulo:
  sub sp, sp, #16
  stp x29, x30, [sp]
  mov x29, sp
  sub sp, sp, #32
// Op 0: Assign(a, 0)
  mov x0, #0
  str x0, [sp, #0]
// Op 1: Assign(b, 0)
  mov x0, #0
  str x0, [sp, #8]
// Op 2: Assign(a, 10)
  mov x0, #10
  str x0, [sp, #0]
// Op 3: Assign(b, 3)
  mov x0, #3
  str x0, [sp, #8]
// Op 4: BinaryOp($1 = a % b)
  ldr x0, [sp, #0]
  ldr x1, [sp, #8]
  sdiv x2, x0, x1
  msub x0, x2, x1, x0
  str x0, [sp, #16]
// Op 5: Return($1)
  ldr x0, [sp, #16]
  b .Ltest_modulo_exit
.Ltest_modulo_exit:
  add sp, sp, #32
  ldp x29, x30, [sp]
  add sp, sp, #16
  ret

.globl _test_precedence
.p2align 2
_test_precedence:
  sub sp, sp, #16
  stp x29, x30, [sp]
  mov x29, sp
  sub sp, sp, #16
// Op 0: BinaryOp($1 = 10 % 3)
  mov x0, #10
  mov x1, #3
  sdiv x2, x0, x1
  msub x0, x2, x1, x0
  str x0, [sp, #0]
// Op 1: BinaryOp($2 = 5 + $1)
  mov x0, #5
  ldr x1, [sp, #0]
  add x0, x0, x1
  str x0, [sp, #8]
// Op 2: Return($2)
  ldr x0, [sp, #8]
  b .Ltest_precedence_exit
.Ltest_precedence_exit:
  add sp, sp, #16
  ldp x29, x30, [sp]
  add sp, sp, #16
  ret
