#include "builtin.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PIRX_PRINTF_BUFFER_SIZE 1024

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

PirxSlice PirxString(const char *str) {
  int len = strlen(str);
  PirxSlice slice = {
      .data = (void *)str,
      .size = len,
      .cap = len + 1,
  };
  return slice;
}

void PirxPrintf(PirxSlice fmt, ...) {
  static char static_buffer[PIRX_PRINTF_BUFFER_SIZE];
  char *buffer;

  if (fmt.size + 1 <= PIRX_PRINTF_BUFFER_SIZE) {
    buffer = static_buffer;
  } else {
    buffer = (char *)malloc(fmt.size + 1);
  }

  strncpy(buffer, fmt.data, fmt.size);
  buffer[fmt.size] = '\0';

  va_list args;
  va_start(args, fmt);
  vprintf(buffer, args);
  va_end(args);

  if (buffer != static_buffer) {
    free(buffer);
  }
}

char *PirxCStr(PirxSlice str) { return (char *)str.data; }
