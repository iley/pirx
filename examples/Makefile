EXAMPLES=hello.pirx factorial.pirx fizzbuzz.pirx extern.pirx stdlib.pirx
ASSEMBLY_FILES=$(EXAMPLES:.pirx=.s)
OBJ_FILES=$(EXAMPLES:.pirx=.o)
BIN_FILES=$(EXAMPLES:.pirx=)

default: $(BIN_FILES)

$(BIN_FILES): %: %.o
	ld -o $@ $< ../stdlib/libpirx.a -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -arch arm64

$(OBJ_FILES): %.o: %.s
	as -arch arm64 -o $@ $<

$(ASSEMBLY_FILES): %.s: %.pirx always
	go run github.com/iley/pirx/cmd/pirxc -o $@ $<

clean:
	rm -f $(BIN_FILES) $(OBJ_FILES) $(ASSEMBLY_FILES)

.PHONY: always default clean
