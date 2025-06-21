package codegen

import (
	"fmt"
	"io"

	"github.com/iley/pirx/internal/codegen/amd64_darwin"
	"github.com/iley/pirx/internal/ir"
)

type Target int

const (
	TargetARM64Darwin Target = iota
)

func TargetFromName(name string) (Target, error) {
	switch name {
	case "arm64-darwin":
		return TargetARM64Darwin, nil
	}
	return 0, fmt.Errorf("unknown target: %s", name)
}

func Generate(output io.Writer, target Target, irp ir.IrProgram) error {
	switch target {
	case TargetARM64Darwin:
		return amd64_darwin.Generate(output, irp)
	}
	return fmt.Errorf("Unknown target: %v", target)
}
