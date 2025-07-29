#include "builtin.h"

#include <stdlib.h>

void *Pirx_Alloc(int size) { return malloc(size); }

void Pirx_Dispose(void *ptr) { free(ptr); }

Pirx_Slice Pirx_Slice_Alloc(int elem_size, int size, int cap) {
  // TODO: Validate size and cap.
  Pirx_Slice slice = {
    .data = Pirx_Alloc(elem_size * cap),
    .size = size,
    .cap = cap,
  };
  return slice;
}

void Pirx_Slice_Dispose(Pirx_Slice slice) {
  Pirx_Dispose(slice.data);
}
