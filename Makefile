default: pirx

pirx: always
	go build -o pirx ./cmd/pirx

testrunner: always
	go build -o testrunner ./cmd/testrunner

tests: pirx testrunner
	./testrunner

always:

clean:
	$(MAKE) -C ./examples clean
	rm -f ./pirx

examples:
	$(MAKE) -C ./examples

.PHONY: always clean default examples tests
