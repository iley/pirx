#include <stdint.h>

// Seven int-class args spill g to the stack while xmm0/xmm1 stay free; both
// doubles must still arrive in float registers (exhaustion is per class).
double mix7i2d(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e,
               int32_t f, int32_t g, double x, double y) {
  return (double)(1 * a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g) +
         10.0 * x + 100.0 * y;
}

// The ninth double lands on the stack; storing it must not clobber the first
// argument in xmm0.
double sum9d(double a, double b, double c, double d, double e, double f,
             double g, double h, double i) {
  return 1 * a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h + 9 * i;
}

// Float registers exhausted by a..h, i goes on the stack; j must still use
// the untouched int registers.
double mix8d1i(double a, double b, double c, double d, double e, double f,
               double g, double h, double i, int32_t j) {
  return 1 * a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h +
         9 * i + 10 * j;
}

typedef struct {
  int32_t x;
  int32_t y;
  int32_t z;
} Triple;

// With the int registers exhausted, the 12-byte struct goes on the stack and
// w must land in the next eightbyte slot after it, not right after byte 12.
int32_t s12stack(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e,
                 int32_t f, int32_t g, int32_t h, Triple s, int32_t w) {
  return a + b + c + d + e + f + g + h + 10 * s.x + 11 * s.y + 12 * s.z +
         100 * w;
}

typedef struct {
  int64_t p;
  int64_t q;
} Pair64;

// Only one int register remains for the 16-byte struct, so it spills to the
// stack; on x86-64 SysV w must back-fill the remaining register (r9).
int64_t backfill(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e,
                 Pair64 s, int32_t w) {
  return a + b + c + d + e + 10 * s.p + 11 * s.q + 100 * w;
}
