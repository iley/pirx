all: pirx examples

pirx: always
	go build -o pirx ./cmd/pirx

always:

clean:
	$(MAKE) -C ./examples clean
	rm -f ./pirx

examples:
	$(MAKE) -C ./examples

.PHONY: always clean examples
