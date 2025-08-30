package common

import (
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/ir"
)

type CodeGenerator interface {
	Generate(ir.IrProgram) (asm.Program, error)
	Format(io.Writer, asm.Program)
}
