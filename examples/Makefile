EXAMPLES=hello.pirx
ASSEMBLY_FILES=$(EXAMPLES:.pirx=.s)
OBJ_FILES=$(EXAMPLES:.pirx=.o)
BIN_FILES=$(EXAMPLES:.pirx=)

default: $(BIN_FILES)

$(BIN_FILES): %: %.o
	ld -o $@ $< -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64

$(OBJ_FILES): %.o: %.s
	as -arch arm64 -o $@ $<

$(ASSEMBLY_FILES): %.s: %.pirx
	go run github.com/iley/pirx/cmd/pirx -o $@ $<

clean:
	rm -f $(BIN_FILES) $(OBJ_FILES) $(ASSEMBLY_FILES)

.PHONY: default clean
