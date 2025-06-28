package opt

import (
	"slices"

	"github.com/iley/pirx/internal/ir"
)

// eliminateIneffectiveAssignments removes ineffective variable assignments.
// An assingment is ineffective if either the variable is never used or it's not used before the next assignment happens.
func eliminateIneffectiveAssignments(irp ir.IrProgram) ir.IrProgram {
	res := ir.IrProgram{
		Functions: slices.Clone(irp.Functions),
	}
	for i := range res.Functions {
		res.Functions[i].Ops = eliminateIneffectiveAssignmentOps(res.Functions[i].Ops)
	}
	return res
}

func eliminateIneffectiveAssignmentOps(ops []ir.Op) []ir.Op {
	res := []ir.Op{}
	for i, op := range ops {
		if !isIneffectiveAssignment(ops, i) {
			res = append(res, op)
		}
	}
	return res
}

func isIneffectiveAssignment(ops []ir.Op, index int) bool {
		first, ok := ops[index].(ir.Assign)
		if !ok {
			return false // not an assignment
		}

		// Find the next assingment.
		// If not found, default to end of the function.
		nextIdx := len(ops)
		for j := index+1; j < len(ops); j++ {
			if b, ok := ops[j].(ir.Assign); ok && b.Target == first.Target {
				nextIdx = j
				break
			}
		}

		for j := index+1; j < nextIdx; j++ {
			if _, ok := ops[j].(ir.Jump); ok {
				// It's a jump, all bets are off.
				return false
			}

			if _, ok := ops[j].(ir.JumpUnless); ok {
				// It's a jump, all bets are off.
				return false
			}

			args := ops[j].GetArgs()
			for _, arg := range args {
				if arg.Variable != "" && arg.Variable == first.Target {
					return false
				}
			}
		}

		return true
}
