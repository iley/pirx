all: pirx examples

pirx: always
	go build -o pirx ./cmd/pirx

testrunner: always
	go build -o testrunner ./cmd/testrunner

always:

clean:
	$(MAKE) -C ./examples clean
	rm -f ./pirx

examples:
	$(MAKE) -C ./examples

tests: pirx testrunner
	./testrunner

.PHONY: always clean examples tests
