package aarch64_darwin

import (
	_ "embed"
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/codegen/aarch64"
	"github.com/iley/pirx/internal/ir"
)

type CodeGenerator struct{}

func (cg *CodeGenerator) Generate(p ir.IrProgram) (asm.Program, error) {
	return aarch64.Generate(p)
}

func (cg *CodeGenerator) Format(out io.Writer, p asm.Program) {
	formatProgram(out, p)
}
