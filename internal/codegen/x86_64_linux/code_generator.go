package x86_64_linux

import (
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/codegen/x86_64"
	"github.com/iley/pirx/internal/ir"
)

type CodeGenerator struct{}

func (cg *CodeGenerator) Generate(program ir.IrProgram) (asm.Program, error) {
	features := x86_64.Features{
		VarargsOnStack:       false,
		FuncLabelsUnderscore: false,
	}
	return x86_64.Generate(program, features)
}

func (cg *CodeGenerator) Optimize(p asm.Program) asm.Program {
	return x86_64.Optimize(p)
}

func (cg *CodeGenerator) Format(out io.Writer, p asm.Program) {
	formatProgram(out, p)
}
