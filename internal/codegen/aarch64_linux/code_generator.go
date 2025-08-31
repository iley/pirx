package aarch64_linux

import (
	_ "embed"
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/codegen/aarch64"
	"github.com/iley/pirx/internal/ir"
)

type CodeGenerator struct{}

func (cg *CodeGenerator) Generate(program ir.IrProgram) (asm.Program, error) {
	features := aarch64.Features{
		VarargsOnStack:       false,
		FuncLabelsUnderscore: false,
	}
	return aarch64.Generate(program, features)
}

func (cg *CodeGenerator) Optimize(p asm.Program) asm.Program {
	return aarch64.Optimize(p)
}

func (cg *CodeGenerator) Format(out io.Writer, p asm.Program) {
	formatProgram(out, p)
}
