EXAMPLES=hello.pirx factorial.pirx fizzbuzz.pirx extern.pirx rule110.pirx
BIN_FILES=$(EXAMPLES:.pirx=)

default: $(BIN_FILES)

$(BIN_FILES): %: %.pirx
	../pirx build -o $@ $<

clean:
	rm -f $(BIN_FILES)

.PHONY: always default clean
