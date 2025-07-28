GO_SOURCES := $(shell find . -type f -name '*.go' ! -name '*_test.go' -not -path './vendor/*')
GO_MODULE_FILES := go.mod go.sum

default: pirx pirxc testrunner

pirx: $(GO_SOURCES) $(GO_MODULE_FILES) pirxc
	go build -mod=vendor -o pirx ./cmd/pirx

pirxc: $(GO_SOURCES) $(GO_MODULE_FILES) stdlib
	go build -mod=vendor -o pirxc ./cmd/pirxc

testrunner: pirx
	go build -mod=vendor -o testrunner ./cmd/testrunner

test: gotests testall

citest: gotests testall testall_o0

gotests:
	go test ./...

testall: testrunner
	@echo " *** Running all end-to-end tests"
	./testrunner -j 8 testall

testall_o0: testrunner
	@echo " *** Running all end-to-end tests with no optimization"
	./testrunner -j 8 testall -O0

clean:
	$(MAKE) -C ./stdlib clean
	$(MAKE) -C ./examples clean
	rm -f ./pirx ./pirxc

examples:
	$(MAKE) -C ./examples

stdlib:
	$(MAKE) -C ./stdlib

.PHONY: clean default examples test stdlib
