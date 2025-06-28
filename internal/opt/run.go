package opt

import "github.com/iley/pirx/internal/ir"

func Run(irp ir.IrProgram) ir.IrProgram {
	irp = eliminateIneffectiveAssignments(irp)
	return irp
}
