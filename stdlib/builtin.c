#include "builtin.h"

#include <stdio.h>
#include <stdlib.h>

void *Pirx_Alloc(int size) { return calloc(size, 1); }

void Pirx_Dispose(void *ptr) { free(ptr); }

Pirx_Slice Pirx_Slice_Alloc(int elem_size, int size, int cap) {
  Pirx_Slice slice = {
      .data = Pirx_Alloc(elem_size * cap),
      .size = size,
      .cap = cap,
  };
  return slice;
}

void Pirx_Slice_Dispose(Pirx_Slice slice) { Pirx_Dispose(slice.data); }

void Pirx_Slice_Resize(Pirx_Slice *slice_ptr, int size) {
  int new_cap = slice_ptr->cap;
  while (new_cap < size) {
    new_cap *= 2;
  }

  slice_ptr->data = realloc(slice_ptr->data, new_cap);
  slice_ptr->size = size;
  slice_ptr->cap = new_cap;
}

void *Pirx_Slice_Ptr(Pirx_Slice slice) { return slice.data; }

int Pirx_Slice_Size(Pirx_Slice slice) { return slice.size; }

int Pirx_Slice_Cap(Pirx_Slice slice) { return slice.cap; }
