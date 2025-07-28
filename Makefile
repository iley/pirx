GO_SOURCES := $(shell find . -type f -name '*.go' ! -name '*_test.go' -not -path './vendor/*')
GO_MODULE_FILES := go.mod go.sum

default: pirx pirxc testrunner

pirx: $(GO_SOURCES) $(GO_MODULE_FILES)
	go build -o pirx ./cmd/pirx

pirxc: $(GO_SOURCES) $(GO_MODULE_FILES) stdlib
	go build -o pirxc ./cmd/pirxc

testrunner: pirx
	go build -o testrunner ./cmd/testrunner

test: testrunner
	go test ./...
	./testrunner -j 8 testall

clean:
	$(MAKE) -C ./stdlib clean
	$(MAKE) -C ./examples clean
	rm -f ./pirx ./pirxc

examples:
	$(MAKE) -C ./examples

stdlib:
	$(MAKE) -C ./stdlib

.PHONY: clean default examples test stdlib
