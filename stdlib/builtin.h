#ifndef _PIRX_STDLIB_BUILTIN_H
#define _PIRX_STDLIB_BUILTIN_H

#include <stdint.h>

extern void Panic();

// Memory management.

// Allocate memory. Auto-generated for new(Type).
extern void *PirxAlloc(int32_t size);

// Free memory. Called directly by user as dispose().
extern void PirxDispose(void *);

// Slices.

typedef struct {
  void *data;
  int32_t size;
  int32_t cap;
} PirxSlice;

extern PirxSlice PirxSliceAlloc(int32_t elem_size, int32_t size, int32_t cap);
extern void PirxSliceDispose(PirxSlice slice);
extern void PirxSliceResize(PirxSlice *slice_ptr, int32_t size);
extern void *PirxSlicePtr(PirxSlice slice);
extern int32_t PirxSliceSize(PirxSlice slice);
extern int32_t PirxSliceCap(PirxSlice slice);

// Type conversion functions.
extern int32_t PirxIntFromInt(int32_t value);
extern int32_t PirxIntFromInt8(int8_t value);
extern int32_t PirxIntFromInt64(int64_t value);
extern int32_t PirxIntFromFloat32(float value);
extern int32_t PirxIntFromFloat64(double value);

extern PirxSlice PirxStr(const char *str);

#endif // _PIRX_STDLIB_BUILTIN_H
