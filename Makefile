UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
else
  FORMAT=win
  TARGET=-target i686-pc-mingw32
endif
endif

EXT=garter

PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind -tag thread
CLANG=clang $(TARGET) -mstackrealign -g -m32 

OBJ_FILES=gctest.o gc.o

CUTEST_DIR=cutest-1.5
CUTEST_OBJ_FILES=$(CUTEST_DIR)/CuTest.o

ALL_OBJ_FILES=$(OBJ_FILES) $(CUTEST_OBJ_FILES)

.PHONY: clean

all: main test gctest

main: main.ml compile.ml runner.ml expr.ml instruction.ml parser.mly lexer.mll misc.ml
	$(BUILD) -no-hygiene -package $(PKGS) main.native
	mv main.native main

test: compile.ml runner.ml test.ml expr.ml instruction.ml parser.mly lexer.mll myTests.ml misc.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

main.o: main.c
	$(CLANG) -c -o $@ $<

output/%.run: output/%.o main.o gc.o
	$(CLANG) -o $@ $^

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.$(EXT) main
	./main $< > $@

$(OBJ_FILES): %.o: %.c gc.h
	$(CLANG) -c $< -o $@ 

$(CUTEST_OBJ_FILES): %.o: %.c %.h
	$(CLANG) -c $< -o $@

gctest: $(ALL_OBJ_FILES) $(CUTEST_DIR)/CuTest.h
	$(CLANG) $(CUTEST_DIR)/AllTests.c $(ALL_OBJ_FILES) -o $@ 

clean:
	rm -rf *.o cutest-1.5/*.o output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
	rm -f main test gctest
