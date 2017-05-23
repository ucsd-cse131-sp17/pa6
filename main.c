#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/queue.h>
#include "gc.h"

extern int our_code_starts_here()    asm("our_code_starts_here");
extern void error()                  asm("error");
extern int print(int val)            asm("print");
extern int equal(int val1, int val2) asm("equal");
extern int input(int i)              asm("input");

extern int* HEAP_END     asm("HEAP_END");
extern int* STACK_BOTTOM asm("STACK_BOTTOM");

extern int* try_gc(int* alloc_ptr, int amount_needed, int* current_ESP, int* stack_top) asm("try_gc");

const int TRUE  = 0xFFFFFFFF;
const int FALSE = 0x7FFFFFFF;

int  INPUT_COUNT = 0;
int* INPUTS      = NULL;

const long int INT_MIN = - (1 << 30);
const long int INT_MAX = (1 << 30) - 1;

// -------------------------------------------
// Global variables used in garbage collection
// -------------------------------------------
size_t HEAP_SIZE;
int* STACK_BOTTOM;
int* HEAP;
int* HEAP_END;
// -------------------------------------------

int equal(int val1, int val2) {
  if(val1 == val2) { return TRUE; }
  else { return FALSE; }
}

void print_rec(int val) {
  if((val & 0x00000001) ^ 0x00000001) {
    printf("%d", val >> 1);
  }
  else if(val == 0xFFFFFFFF) {
    printf("true");
  }
  else if(val == 0x7FFFFFFF) {
    printf("false");
  }
  else if((val & 0x00000003) == 1) {
    int* valp = (int*) (val - 1);

    if(*valp) {
      printf("<cyclic tuple>");
      return;
    }
    *(valp) += 1;
    printf("(");
    print_rec(*(valp + 1));
    printf(",");
    print_rec(*(valp + 2));
    printf(")");
    *(valp) -= 1;
    fflush(stdout);
  }
  else {
    printf("Unknown value: %#010x", val);
  }
}


int print(int val) {
  print_rec(val);
  printf("\n");
  return val;
}

void error(int i) {
  if (i == 1) {
    fprintf(stderr, "Error: expected a number");
  }
  else if (i == 2) {
    fprintf(stderr, "Error: expected a boolean");
  }
  else if (i == 3) {
    fprintf(stderr, "Error: Integer overflow");
  }
  else if (i == 4) {
    fprintf(stderr, "Error: expected a pair");
  }
  else {
    fprintf(stderr, "Error: Unknown error code: %d\n", i);
    exit(1);
  }

  exit(i);
}

int input(int i) {
  i = i >> 1;

  if (i < 0 || i >= INPUT_COUNT) {
    fprintf(stderr, "input index out of bounds (given:%d #args:%d) \n", i, INPUT_COUNT);
    exit(1);
  }
  
  return INPUTS[i];
}

int parse_input(const char* in) {
  if (strcmp(in, "true") == 0) {
    return TRUE;

  } else if (strcmp(in, "false") == 0) {
    return FALSE;

  } else {
    size_t l = strlen(in);
    if (l == 0) {
      fprintf(stderr, "input is empty\n");
      exit(1);
    }
      
    char* endptr = (char*) &in[l];
    long int r = strtol(in, &endptr, 10);
    
    if (*endptr != '\0') {
      fprintf(stderr, "input '%s' is not a number or a boolean\n", in);
      exit(1);
    }

    if (r < INT_MIN || r > INT_MAX) {
      fprintf(stderr, "overflow: input '%s' is not a representable number\n", in);
      exit(1);
    }
      
    return (int) r * 2;
  }
}

/*
  Try to clean up space in memory by calling gc.

  Arguments:

    1. alloc_ptr:    The current value of EBX (where the next value would be
                     allocated without GC)
    2. bytes_needed: The number of bytes that the runtime is trying to allocate
    3. current_ESP:  The current value of ESP (for tracking stack information)
    4. stack_top:    Pointer to the last variable on the stack

  Returns:

    The new value for EBX, for the runtime to start using as the allocation
    point.  Must be set to a location that provides enough room to fit
    bytes_allocated more bytes in the given heap space
*/
int* try_gc(int* alloc_ptr, int bytes_needed, int* current_ESP, int* stack_top) {
  if(HEAP == alloc_ptr) {
    fprintf(stderr,
            "out of memory: Allocation of %d words too large for %d-word heap\n",
            bytes_needed / 4, 
            (int) HEAP_SIZE);
    exit(1);
  }

  // When you're confident in your collector, delete the next line and uncomment
  // the second line to trigger your GC
  int* new_ebx = alloc_ptr;
  // int* new_ebx = gc(STACK_BOTTOM, current_ESP, stack_top, HEAP, HEAP_END);

  if((new_ebx + (bytes_needed / 4)) > HEAP_END) {
    fprintf(stderr, 
            "out of memory: Needed %d words, but only %d remain after collection",
            bytes_needed / 4, 
            HEAP_END - new_ebx);
    exit(1);
  }
  
  return new_ebx;
}

int main(int argc, char** argv) {
  if(argc > 1) {
    HEAP_SIZE = atoi(argv[1]);
  } else {
    HEAP_SIZE = 100000;
  }
  
  INPUT_COUNT = argc > 2 ? argc - 2 : 0;

  if (INPUT_COUNT > 0) {
    INPUTS = calloc(INPUT_COUNT, sizeof(int));

    int i = 0;
    for (; i < argc - 2; i++) {
      INPUTS[i] = parse_input(argv[i+2]);
    }
  }

  HEAP = calloc(HEAP_SIZE, sizeof (int));

  if (HEAP == NULL) {
    fprintf(stderr, "HEAP is null\n");
    exit(1);
  } else if (((int) HEAP) & 0x3) {
    fprintf(stderr, "last 2 bits of HEAP is not 0 !\n");
    exit(1);
  }

  HEAP_END = HEAP + HEAP_SIZE;
  
  int result = our_code_starts_here(HEAP);

  print(result);

  return 0;
}

