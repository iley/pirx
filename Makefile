GO_SOURCES := $(shell find . -type f -name '*.go' ! -name '*_test.go' -not -path './vendor/*')
GO_MODULE_FILES := go.mod go.sum
TEST_CONCURRENCY := 16

default: pirx pirxc testrunner stdlib

pirx: $(GO_SOURCES) $(GO_MODULE_FILES)
	go build -mod=vendor -o pirx ./cmd/pirx

pirxc: $(GO_SOURCES) $(GO_MODULE_FILES)
	go build -mod=vendor -o pirxc ./cmd/pirxc

testrunner: pirx pirxc stdlib
	go build -mod=vendor -o testrunner ./cmd/testrunner

test: gotests testall

citest: gotests testall testall_o0

gotests:
	go test ./...

testall: testrunner
	@echo " *** Running all end-to-end tests"
	./testrunner -j $(TEST_CONCURRENCY) testall

testall_o0: testrunner
	@echo " *** Running all end-to-end tests with no optimization"
	./testrunner -j $(TEST_CONCURRENCY) testall -O0

# Pattern rule for running individual tests: make test_046 runs ./testrunner test 046
test_%: testrunner
	./testrunner test $*

clean:
	$(MAKE) -C ./stdlib clean
	$(MAKE) -C ./examples clean
	rm -f ./pirx ./pirxc ./testrunner

examples:
	$(MAKE) -C ./examples

stdlib:
	$(MAKE) -C ./stdlib

lint:
	golangci-lint run

fmt:
	golangci-lint fmt

cloc:
	cloc . --exclude-dir=vendor

# Docker targets for testing the Linux backend from a MacOS host.
docker-test:
	docker build -t pirx-linux .
	docker run --rm -it pirx-linux

docker-shell:
	docker build -t pirx-linux .
	docker run --rm -it pirx-linux /bin/bash

# Docker targets for testing specific architectures.
docker-test-arm64:
	docker build --platform linux/arm64 -t pirx-linux-arm64 .
	docker run --rm --platform linux/arm64 pirx-linux-arm64

docker-test-x86:
	docker build --platform linux/amd64 -t pirx-linux-x86 .
	docker run --rm --platform linux/amd64 pirx-linux-x86

docker-shell-x86:
	docker build --platform linux/amd64 -t pirx-linux-x86 .
	docker run --rm -it --platform linux/amd64 pirx-linux-x86 /bin/bash

.PHONY: clean default examples test stdlib docker-test docker-shell docker-test-arm64 docker-test-x86 docker-shell-x86
