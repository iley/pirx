package common

import (
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/ir"
)

type CodeGenerator interface {
	Generate(ir.IrProgram) (asm.Program, error)
	Optimize(asm.Program) asm.Program
	Format(io.Writer, asm.Program)
}
