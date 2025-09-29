#include "builtin.h"

#include <stdio.h>
#include <stdlib.h>

void *PirxAlloc(int32_t size) { return calloc(size, 1); }

void PirxDispose(void *ptr) { free(ptr); }

PirxSlice PirxSliceAlloc(int32_t elem_size, int32_t size, int32_t cap) {
  PirxSlice slice = {
      .data = PirxAlloc(elem_size * cap),
      .size = size,
      .cap = cap,
  };
  return slice;
}

void PirxSliceDispose(PirxSlice slice) { PirxDispose(slice.data); }

void PirxSliceResize(PirxSlice *slice_ptr, int32_t size) {
  int32_t new_cap = slice_ptr->cap;
  while (new_cap < size) {
    new_cap *= 2;
  }

  slice_ptr->data = realloc(slice_ptr->data, new_cap);
  slice_ptr->size = size;
  slice_ptr->cap = new_cap;
}

void *PirxSlicePtr(PirxSlice slice) { return slice.data; }

int32_t PirxSliceSize(PirxSlice slice) { return slice.size; }

int32_t PirxSliceCap(PirxSlice slice) { return slice.cap; }

int32_t PirxIntFromInt(int32_t value) { return value; }

int32_t PirxIntFromInt8(int8_t value) { return (int32_t)value; }

int32_t PirxIntFromInt64(int64_t value) { return (int32_t)value; }

int32_t PirxIntFromFloat32(float value) { return (int32_t)value; }

int32_t PirxIntFromFloat64(double value) { return (int32_t)value; }
