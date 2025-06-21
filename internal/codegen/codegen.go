package codegen

import (
	"fmt"
	"io"

	"github.com/iley/pirx/internal/codegen/aarch64_darwin"
	"github.com/iley/pirx/internal/ir"
)

type Target int

const (
	TargetAARCH64Darwin Target = iota
)

func TargetFromName(name string) (Target, error) {
	switch name {
	case "aarch64-darwin":
		return TargetAARCH64Darwin, nil
	}
	return 0, fmt.Errorf("unknown target: %s", name)
}

func Generate(output io.Writer, target Target, irp ir.IrProgram) error {
	switch target {
	case TargetAARCH64Darwin:
		return aarch64_darwin.Generate(output, irp)
	}
	return fmt.Errorf("Unknown target: %v", target)
}
