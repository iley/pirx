#include "builtin.h"

#include <stdio.h>
#include <stdlib.h>

void *Pirx_Alloc(int32_t size) { return calloc(size, 1); }

void Pirx_Dispose(void *ptr) { free(ptr); }

Pirx_Slice Pirx_Slice_Alloc(int32_t elem_size, int32_t size, int32_t cap) {
  Pirx_Slice slice = {
      .data = Pirx_Alloc(elem_size * cap),
      .size = size,
      .cap = cap,
  };
  return slice;
}

void Pirx_Slice_Dispose(Pirx_Slice slice) { Pirx_Dispose(slice.data); }

void Pirx_Slice_Resize(Pirx_Slice *slice_ptr, int32_t size) {
  int32_t new_cap = slice_ptr->cap;
  while (new_cap < size) {
    new_cap *= 2;
  }

  slice_ptr->data = realloc(slice_ptr->data, new_cap);
  slice_ptr->size = size;
  slice_ptr->cap = new_cap;
}

void *Pirx_Slice_Ptr(Pirx_Slice slice) { return slice.data; }

int32_t Pirx_Slice_Size(Pirx_Slice slice) { return slice.size; }

int32_t Pirx_Slice_Cap(Pirx_Slice slice) { return slice.cap; }

int32_t Pirx_Int_From_Int(int32_t value) { return value; }

int32_t Pirx_Int_From_Int8(int8_t value) { return (int32_t)value; }

int32_t Pirx_Int_From_Int64(int64_t value) { return (int32_t)value; }

int32_t Pirx_Int_From_Float32(float value) { return (int32_t)value; }

int32_t Pirx_Int_From_Float64(double value) { return (int32_t)value; }
