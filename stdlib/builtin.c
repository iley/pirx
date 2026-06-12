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

void PirxSliceResize(int32_t elem_size, PirxSlice *slice_ptr, int32_t size) {
  int32_t old_cap = slice_ptr->cap;
  int32_t new_cap = old_cap;
  while (new_cap < size) {
    // Doubling a zero capacity would loop forever, so start from 1.
    new_cap = new_cap > 0 ? new_cap * 2 : 1;
  }

  if (new_cap > old_cap) {
    slice_ptr->data = realloc(slice_ptr->data, elem_size * new_cap);
    // Unlike new() which uses calloc, realloc leaves the grown region
    // uninitialized; zero it so new elements read as 0.
    memset((char *)slice_ptr->data + elem_size * old_cap, 0,
           elem_size * (new_cap - old_cap));
  }

  slice_ptr->size = size;
  slice_ptr->cap = new_cap;
}

void *PirxSlicePtr(PirxSlice slice) { return slice.data; }

int32_t PirxSliceSize(PirxSlice slice) { return slice.size; }

int32_t PirxSliceCap(PirxSlice slice) { return slice.cap; }

PirxSlice PirxSliceRange(int32_t elem_size, PirxSlice slice, int32_t start,
                         int32_t end) {
  PirxSlice result = {
      .data = (char *)slice.data + (elem_size * start),
      .size = end - start,
      .cap = slice.cap - start,
  };
  return result;
}

int32_t PirxIntFromInt(int32_t value) { return value; }

int32_t PirxIntFromInt8(int8_t value) { return (int32_t)value; }

int32_t PirxIntFromInt64(int64_t value) { return (int32_t)value; }

int32_t PirxIntFromFloat32(float value) { return (int32_t)value; }

int32_t PirxIntFromFloat64(double value) { return (int32_t)value; }

PirxSlice PirxString(int32_t len, const char *str) {
  PirxSlice slice = {
      .data = (void *)str,
      .size = len,
      .cap = len + 1,
  };
  return slice;
}

// Unlike range() on a slice, range() on a string copies the bytes into a
// fresh buffer instead of producing a view. This maintains the invariant that
// every string's data is NUL-terminated at data[size], which cstr(), open()
// and printf's %s all rely on (varargs lose the size, so the terminator is
// the only length information they get).
PirxSlice PirxStringRange(PirxSlice str, int32_t start, int32_t end) {
  int32_t size = end - start;
  // PirxAlloc zeroes the buffer, so the terminating NUL is already in place.
  char *buffer = PirxAlloc(size + 1);
  if (size > 0) {
    memcpy(buffer, (char *)str.data + start, size);
  }
  PirxSlice result = {.data = buffer, .size = size, .cap = size + 1};
  return result;
}

int32_t PirxStringEq(PirxSlice a, PirxSlice b) {
  if (a.size != b.size) {
    return 0;
  }
  if (a.size == 0) {
    // Empty strings may have NULL data which memcmp doesn't accept.
    return 1;
  }
  return memcmp(a.data, b.data, a.size) == 0;
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

void *PirxOpen(PirxSlice path_str) {
  const char *path = (const char *)path_str.data;
  return fopen(path, "r");
}

PirxSlice PirxReadLine(void *fp) {
  FILE *file = (FILE *)fp;
  if (file == NULL) {
    // A null handle (failed open()) reads as an empty line instead of crashing.
    PirxSlice empty = {.data = NULL, .size = 0, .cap = 0};
    return empty;
  }

  char *line = NULL;
  size_t len = 0;
  ssize_t read = getline(&line, &len, file);

  if (read == -1) {
    if (line) {
      free(line);
    }
    PirxSlice empty = {.data = NULL, .size = 0, .cap = 0};
    return empty;
  }

  if (read > 0 && line[read - 1] == '\n') {
    read--;
  }

  char *buffer = PirxAlloc(read + 1);
  memcpy(buffer, line, read);
  buffer[read] = '\0';
  free(line);

  PirxSlice result = {.data = buffer, .size = read, .cap = read + 1};
  return result;
}

void PirxClose(void *fp) {
  if (fp != NULL) {
    fclose((FILE *)fp);
  }
}
