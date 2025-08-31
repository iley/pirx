package aarch64

import (
	"fmt"

	"github.com/iley/pirx/internal/asm"
)

func Optimize(program asm.Program) asm.Program {
	result := program

	for i, fn := range program.Functions {
		result.Functions[i] = asm.Function{
			Name:  fn.Name,
			Lines: optimizeFunction(fn.Lines),
		}
	}

	return program
}

func optimizeFunction(fn []asm.Line) []asm.Line {
	// TODO: Add more optimizations here.
	return removeIneffectiveJumps(fn)
}

func removeIneffectiveJumps(lines []asm.Line) []asm.Line {
	result := make([]asm.Line, 0, len(lines))

	for i := range lines {
		curr := lines[i]
		if i == len(lines)-1 {
			// No next line.
			result = append(result, curr)
			continue
		}

		next := lines[i+1]
		if isIneffectiveJump(curr, next) {
			continue
		}

		result = append(result, curr)
	}

	return result
}

func isIneffectiveJump(curr, next asm.Line) bool {
	if curr.Op != "b" {
		return false
	}

	label := curr.Arg1.Label
	if label == "" {
		panic(fmt.Errorf("invalid jump instruction, label missing: %#v", curr))
	}

	if next.Label != label {
		return false
	}

	return true
}
