#include <stdint.h>

typedef struct {
  char f1;
  char f2;
  char f3;
  char f4;
  char f5;
  char f6;
  char f7;
  char f8;
  char f9;
} struct_9;

typedef struct {
  int64_t first_8_bytes;
  char f9;
  char f10;
  char f11;
  char f12;
  char f13;
  char f14;
  char f15;
} struct_15;

struct_9 returns_9_bytes() {
  struct_9 value = {
      .f1 = 1,
      .f2 = 2,
      .f3 = 3,
      .f4 = 4,
      .f5 = 5,
      .f6 = 6,
      .f7 = 7,
      .f8 = 8,
      .f9 = 9,
  };
  return value;
}

struct_15 returns_15_bytes() {
  struct_15 value = {
      .first_8_bytes = 0x0102030405060708,
      .f9 = 9,
      .f10 = 10,
      .f11 = 11,
      .f12 = 12,
      .f13 = 13,
      .f14 = 14,
      .f15 = 15,
  };
  return value;
}
