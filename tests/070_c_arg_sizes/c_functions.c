#include <stdint.h>
#include <stdio.h>

typedef struct {
  int32_t a;
  int32_t b;
  int32_t c;
} struct_12;

typedef struct {
  int64_t a;
  int64_t b;
} struct_16;

void print_struct_12(struct_12 s) {
  printf("a = 0x%08x b = 0x%08x c = 0x%08x\n", s.a, s.b, s.c);
}

void print_struct_16(struct_16 s) {
  printf("a = 0x%016llx b = 0x%016llx\n", s.a, s.b);
}

void print_struct_16_extra_args(int x, struct_16 s, int y) {
  printf("x = %d y = %d\n", x, y);
  printf("a = 0x%016llx b = 0x%016llx\n", s.a, s.b);
}
