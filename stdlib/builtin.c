#include "builtin.h"

#include <stdlib.h>

void *Pirx_Alloc(int size) { return malloc(size); }

void Pirx_Dispose(void *ptr) { free(ptr); }
