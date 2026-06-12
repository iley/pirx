#include <stdint.h>

typedef struct {
  int64_t x;
  int64_t y;
} Pair;

// Three leading ints push the by-value struct into the rcx:r8 register pair
// on x86_64 (x3:x4 on arm64). Distinct coefficients catch halves landing in
// the wrong slot.
int64_t pair_sum(int32_t a, int32_t b, int32_t c, Pair p) {
  return a + b + c + 10 * p.x + 100 * p.y;
}
