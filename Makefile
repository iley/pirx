default: pirx testrunner

pirx: stdlib always
	go build -o pirx ./cmd/pirx

testrunner: always
	go build -o testrunner ./cmd/testrunner

test: pirx testrunner
	go test ./...
	./testrunner -j 8 testall

always:

clean:
	$(MAKE) -C ./stdlib clean
	$(MAKE) -C ./examples clean
	rm -f ./pirx

examples:
	$(MAKE) -C ./examples

stdlib:
	$(MAKE) -C ./stdlib

.PHONY: always clean default examples test stdlib
