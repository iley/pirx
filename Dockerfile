FROM golang:1.25-alpine

# Install necessary tools for compilation
RUN apk add --no-cache \
    gcc \
    musl-dev \
    make \
    bash \
    curl

# Install golangci-lint
RUN curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.54.2

# Set working directory
WORKDIR /app

# Copy go mod files first for better caching
COPY go.mod go.sum ./
RUN go mod download

# Copy source code
COPY . .

# Build the project
RUN make

# Default command runs tests
CMD ["make", "test"]
