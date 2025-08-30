package codegen

import (
	"fmt"
	"io"

	"github.com/iley/pirx/internal/codegen/aarch64_darwin"
	"github.com/iley/pirx/internal/codegen/aarch64_linux"
	"github.com/iley/pirx/internal/codegen/common"
	"github.com/iley/pirx/internal/ir"
)

type Target int

const (
	TargetAARCH64Darwin Target = iota
	TargetAARCH64Linux  Target = iota
)

func TargetFromName(name string) (Target, error) {
	switch name {
	case "aarch64-darwin":
		return TargetAARCH64Darwin, nil
	case "aarch64-linux":
		return TargetAARCH64Linux, nil
	}
	return 0, fmt.Errorf("unknown target: %s", name)
}

func Generate(out io.Writer, target Target, irp ir.IrProgram) error {
	var cg common.CodeGenerator
	switch target {
	case TargetAARCH64Darwin:
		cg = &aarch64_darwin.CodeGenerator{}
	case TargetAARCH64Linux:
		cg = &aarch64_linux.CodeGenerator{}
	default:
		return fmt.Errorf("unknown target: %v", target)
	}

	asmProgram, err := cg.Generate(irp)
	if err != nil {
		return err
	}

	// TODO: Insert the optimization step here.

	cg.Format(out, asmProgram)
	return nil
}
