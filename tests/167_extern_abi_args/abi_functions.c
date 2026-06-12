#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

// Nine ints exhaust the integer argument registers and push i onto the stack;
// the double must still arrive in the first float register.
double mix9i1d(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e,
               int32_t f, int32_t g, int32_t h, int32_t i, double x) {
  return (double)(a + b + c + d + e + f + g + h + i) + x;
}

// Args i and j land on the stack; Apple packs them at 4 bytes each.
// Distinct coefficients catch arguments landing in the wrong slot.
int32_t sum10(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e,
              int32_t f, int32_t g, int32_t h, int32_t i, int32_t j) {
  return 1 * a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h +
         9 * i + 10 * j;
}

// Both register classes get exhausted (8 doubles + 8 ints on arm64) and
// t1..t5 land on the stack with mixed sizes and alignment gaps.
double mixmany(double d1, double d2, double d3, double d4, double d5,
               double d6, double d7, double d8, int32_t i1, int32_t i2,
               int32_t i3, int32_t i4, int32_t i5, int32_t i6, int32_t s1,
               int32_t s2, int8_t t1, int32_t t2, bool t3, int64_t t4,
               int32_t t5) {
  printf("t1=%d t2=%d t3=%d t4=%lld t5=%d\n", t1, t2, t3, (long long)t4, t5);
  return 1 * d1 + 2 * d2 + 3 * d3 + 4 * d4 + 5 * d5 + 6 * d6 + 7 * d7 +
         8 * d8 + 9 * i1 + 10 * i2 + 11 * i3 + 12 * i4 + 13 * i5 + 14 * i6 +
         15 * s1 + 16 * s2 + 17 * t1 + 18 * t2 + 19 * t3 + 20 * t4 + 21 * t5;
}
