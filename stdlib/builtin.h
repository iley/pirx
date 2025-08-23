#ifndef _PIRX_STDLIB_BUILTIN_H
#define _PIRX_STDLIB_BUILTIN_H

#include <stdint.h>

extern void Panic();

// Memory management.

// Allocate memory. Auto-generated for new(Type).
extern void *Pirx_Alloc(int size);

// Free memory. Called directly by user as dispose().
extern void Pirx_Dispose(void *);

// Slices.

typedef struct {
  void *data;
  int32_t size;
  int32_t cap;
} Pirx_Slice;

extern Pirx_Slice Pirx_Slice_Alloc(int elem_size, int size, int cap);

extern void Pirx_Slice_Dispose(Pirx_Slice slice);

#endif // _PIRX_STDLIB_BUILTIN_H
