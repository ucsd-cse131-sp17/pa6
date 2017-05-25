#include <stdio.h>
#include <stdlib.h>
#include "gc.h"

int debug(const char *format, ...) {
  #ifdef ENABLE_DEBUG
  va_list args;
  va_start(args, format);

  int rc = vfprintf(stderr, format, args);
  fflush(stderr);
  return rc;
  #else
  return 0;
  #endif
}

void debug_print_heap(int* heap, int size) {
  for(int i = 0; i < size; i += 1) {
    debug("  %3d/%p: %-10p (%d)\n", i, (heap + i), (int*)(*(heap + i)), *(heap + i));
  }
}

int* mark(int* stack_top, int* current_ESP, int* stack_bottom, int* heap_start) {
  fprintf(stderr, "TODO: mark @ gc.c\n");
  exit(1);
}

void forward(int* stack_top, int* current_ESP, int* stack_bottom, int* heap_start, int* max_address) {
  fprintf(stderr, "TODO: forward @ gc.c\n");
  exit(1);
}

int* compact(int* heap_start, int* max_address, int* heap_end) {
  fprintf(stderr, "TODO: compact @ gc.c\n");
  exit(1);
}

int* gc(int* stack_bottom, int* current_ESP, int* stack_top, int* heap_start, int* heap_end) {
  int* max_address = mark(stack_top, current_ESP, stack_bottom, heap_start);

  forward(stack_top, current_ESP, stack_bottom, heap_start, max_address);

  int* answer = compact(heap_start, max_address, heap_end);

  return answer;
}

