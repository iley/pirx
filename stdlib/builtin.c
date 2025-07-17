#include "builtin.h"

#include <stdio.h>
#include <stdlib.h>

void dispose(void *ptr) {
  free(ptr);
}
