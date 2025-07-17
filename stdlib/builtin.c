#include "builtin.h"

#include <stdlib.h>

void dispose(void *ptr) { free(ptr); }
