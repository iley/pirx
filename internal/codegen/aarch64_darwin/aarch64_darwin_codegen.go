package aarch64_darwin

import (
	_ "embed"
	"io"

	"github.com/iley/pirx/internal/codegen/aarch64"
	"github.com/iley/pirx/internal/ir"
)

func Generate(output io.Writer, irp ir.IrProgram) error {
	asmProgram, err := aarch64.Generate(irp)
	if err != nil {
		return err
	}

	formatProgram(output, asmProgram)

	return nil
}
