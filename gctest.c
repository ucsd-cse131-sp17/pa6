#include "gc.h"
#include "cutest-1.5/CuTest.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void CuAssertArrayEquals_LineMsg(CuTest* tc, 
                                 const char* file, 
                                 int line, 
                                 const char* message, 
                                 int* expected, 
                                 int* actual, 
                                 int length);

#define CuAssertArrayEquals(tc,expect,actual,len) CuAssertArrayEquals_LineMsg((tc),__FILE__,__LINE__,NULL,(expect),(actual),(len))

void CuAssertArrayEquals_LineMsg(CuTest* tc,
                                 const char* file,
                                 int line,
                                 const char* message,
                                 int* expected,
                                 int* actual,
                                 int length) {
  char buf[STRING_MAX];
  int i = 0;
  int found_mismatch = 0;

  int actv;
  int expv;

  for(i = 0; i < length; i += 1) {
    expv = *(expected + i);
    actv = *(actual + i);
    if(expv != actv) {
      sprintf(buf, "values at index %d differ: expected %#010x but was %#010x", i, expv, actv);
      found_mismatch = 1;
    }
  }
  if(found_mismatch) {
    CuFail_Line(tc, file, line, message, buf);
  }
}


void TestArray(CuTest* tc) {
  int stack[3]  = {1, 2, 3};
  int expect[3] = {1, 2, 3};

  CuAssertArrayEquals(tc, expect, stack, 3);
}

void TestMark(CuTest* tc) {
  unsigned heap_size = 12;
  int* heap = calloc(heap_size, sizeof (int));

  heap[0] = 0x00000000; // a pair (that will be collected)
  heap[1] = ((int)(heap + 3)) | 0x00000001; // another pair on the heap
  heap[2] = 0x00000030; // the number 24

  heap[3] = 0x00000000; // a pair
  heap[4] = ((int)(heap + 9)) | 0x00000001; // another pair on the heap
  heap[5] = 0x00000008; // the number 4

  heap[6] = 0x00000000;
  heap[7] = 0x00000004; // the number 2
  heap[8] = 0x00000006; // the number 3

  heap[9]  = 0x00000000;
  heap[10] = 0x0000000a; // the number 5
  heap[11] = 0x0000000c; // the number 6
  
  int stack[5] = {
    ((int)(heap + 3)) + 1, // a reference to the second pair on the heap
    0x0000000e, // the number 7
    0x00000000, // to be filled in with mock esp
    0x00000000, // to be filled in with mock return ptr
    0xffffffff
  };

  stack[2] = (int)(stack + 4); // some address further "down"
  stack[3] = 0x0adadad0;       // mock return ptr/data to skip

  debug("--------------------------------------------------\n");
  debug("1. MARK\n");
  debug("--------------------------------------------------\n");

  int* expectHeap = calloc(heap_size, sizeof (int));
  memcpy(expectHeap, heap, heap_size * (sizeof (int)));
  expectHeap[3] = 0x00000001;
  expectHeap[9] = 0x00000001;

  int* max_address = mark(stack, (stack + 3), (stack + 4), heap);

  CuAssertArrayEquals(tc, expectHeap, heap, heap_size);

  debug("heap: %p, max address: %p\n", heap, max_address);
  debug_print_heap(heap, heap_size);
  debug("#############################\n");
  debug_print_heap(expectHeap, heap_size);

  debug("--------------------------------------------------\n");
  debug("2. FORWARD\n");
  debug("--------------------------------------------------\n");

  int* expectHeap2 = calloc(heap_size, sizeof (int));
  memcpy(expectHeap2, heap, heap_size * (sizeof (int)));

  forward(stack, (stack + 3), stack + 4, heap, max_address);

  expectHeap2[3] = ((int)heap) + 1;
  expectHeap2[4] = ((int)(heap + 3)) + 1;
  expectHeap2[9] = ((int)(heap + 3)) + 1;

  debug_print_heap(heap, heap_size);
  debug("#############################\n");
  debug_print_heap(expectHeap2, heap_size);

  CuAssertArrayEquals(tc, expectHeap2, heap, heap_size);


  debug("--------------------------------------------------\n");
  debug("3. COMPACT\n");
  debug("--------------------------------------------------\n");

  int* expectHeap3 = calloc(heap_size, sizeof (int));

  compact(heap, max_address, heap + heap_size);

  expectHeap3[0]  = 0x00000000; // a pair
  expectHeap3[1]  = ((int)(heap + 3)) | 0x00000001; // another pair on the heap
  expectHeap3[2]  = 0x00000008; // the number 4

  expectHeap3[3]  = 0x00000000;
  expectHeap3[4]  = 0x0000000a; // the number 5
  expectHeap3[5]  = 0x0000000c; // the number 6

  expectHeap3[6]  = 0x0cab005e;
  expectHeap3[7]  = 0x0cab005e;
  expectHeap3[8]  = 0x0cab005e;

  expectHeap3[9]  = 0x0cab005e;
  expectHeap3[10] = 0x0cab005e;
  expectHeap3[11] = 0x0cab005e;

  debug_print_heap(heap, heap_size);
  debug("#############################\n");
  debug_print_heap(expectHeap3, heap_size);

  CuAssertArrayEquals(tc, expectHeap3, heap, heap_size);

  free(heap);
  free(expectHeap);
  free(expectHeap2);
  free(expectHeap3);
}

CuSuite* CuGetSuite(void) {
  CuSuite* suite = CuSuiteNew();

  SUITE_ADD_TEST(suite, TestArray);
  SUITE_ADD_TEST(suite, TestMark);

  return suite;
}

