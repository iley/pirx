#include "builtin.h"

#include <stdio.h>
#include <stdlib.h>

void *Pirx_Alloc(int size) { return malloc(size); }

void Pirx_Dispose(void *ptr) {
  printf("*** disposing %p\n", ptr);
  free(ptr);
}

Pirx_Slice Pirx_Slice_Alloc(int elem_size, int size, int cap) {
  printf("*** allocating slice: elem_size=%d, size=%d, cap=%d\n", elem_size,
         size, cap);
  // TODO: Validate size and cap.
  Pirx_Slice slice = {
      .data = Pirx_Alloc(elem_size * cap),
      .size = size,
      .cap = cap,
  };
  printf("*** allocated %p\n", slice.data);
  return slice;
}

void Pirx_Slice_Dispose(Pirx_Slice slice) { Pirx_Dispose(slice.data); }
