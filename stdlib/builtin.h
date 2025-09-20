#ifndef _PIRX_STDLIB_BUILTIN_H
#define _PIRX_STDLIB_BUILTIN_H

#include <stdint.h>

extern void Panic();

// Memory management.

// Allocate memory. Auto-generated for new(Type).
extern void *Pirx_Alloc(int32_t size);

// Free memory. Called directly by user as dispose().
extern void Pirx_Dispose(void *);

// Slices.

typedef struct {
  void *data;
  int32_t size;
  int32_t cap;
} Pirx_Slice;

extern Pirx_Slice Pirx_Slice_Alloc(int32_t elem_size, int32_t size, int32_t cap);
extern void Pirx_Slice_Dispose(Pirx_Slice slice);
extern void Pirx_Slice_Resize(Pirx_Slice *slice_ptr, int32_t size);
extern void *Pirx_Slice_Ptr(Pirx_Slice slice);
extern int32_t Pirx_Slice_Size(Pirx_Slice slice);
extern int32_t Pirx_Slice_Cap(Pirx_Slice slice);

// Type conversion functions.
extern int32_t Pirx_Int_From_Int(int32_t value);
extern int32_t Pirx_Int_From_Int8(int8_t value);
extern int32_t Pirx_Int_From_Int64(int64_t value);
extern int32_t Pirx_Int_From_Float32(float value);
extern int32_t Pirx_Int_From_Float64(double value);

#endif // _PIRX_STDLIB_BUILTIN_H
