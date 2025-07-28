default: pirx pirxc testrunner

pirx: always
	go build -o pirx ./cmd/pirx

pirxc: stdlib always
	go build -o pirxc ./cmd/pirxc

testrunner: pirx
	go build -o testrunner ./cmd/testrunner

test: testrunner
	go test ./...
	./testrunner -j 8 testall

always:

clean:
	$(MAKE) -C ./stdlib clean
	$(MAKE) -C ./examples clean
	rm -f ./pirx ./pirxc

examples:
	$(MAKE) -C ./examples

stdlib:
	$(MAKE) -C ./stdlib

.PHONY: always clean default examples test stdlib
