package amd64_darwin

import (
	_ "embed"
	"fmt"
	"io"
	"maps"
	"slices"

	"github.com/iley/pirx/internal/ir"
)

const (
	WORD_SIZE = 8
)

//go:embed prologue.txt
var prologue string

func Generate(output io.Writer, irp ir.IrProgram) error {
	io.WriteString(output, prologue)
	for _, f := range irp.Functions {
		err := generateFunction(output, f)
		if err != nil {
			return fmt.Errorf("Error when generating code for function %s: %w", f.Name, err)
		}
	}
	return nil
}

func generateFunction(output io.Writer, f ir.IrFunction) error {
	io.WriteString(output, fmt.Sprintf("%s:\n", f.Name))

	// Map from local variable name to its offset on the stack.
	locals := make(map[string]int)

	// Add params to locals.
	for _, param := range f.Params {
		locals[param] = -1
	}

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range f.Ops {
		if assign, ok := op.(ir.Assign); ok {
			if _, seen := locals[assign.Target]; seen {
				continue
			}
			// We'll assign the actual offset below.
			locals[assign.Target] = -1
		}
	}

	// Allocate space on the stack:
	// * 2 words for FP and LR
	// * 4 words for x19-x22
	// * Local variables including function arguments.
	reservedWords := 2 + 4
	allocWords := reservedWords + len(locals)

	// For now we're going to assume that all variables are 64-bit.
	fmt.Fprintf(output, "  // prologue\n")
	fmt.Fprintf(output, "  stp x29, x30, [sp, -%d]!\n", allocWords*WORD_SIZE)
	fmt.Fprintf(output, "  mov x29, sp\n")
	fmt.Fprintf(output, "  stp x19, x20, [sp, 16]\n")
	fmt.Fprintf(output, "  stp x21, x22, [sp, 32]\n")
	fmt.Fprintf(output, "\n")

	// Offset for locals.
	offset := reservedWords * WORD_SIZE
	for i, param := range f.Params {
		fmt.Fprintf(output, "  str x%d, [sp, %d]\n", i, offset)
		locals[param] = offset
		offset += WORD_SIZE
	}

	for _, name := range slices.Collect(maps.Keys(locals)) {
		if locals[name] != -1 {
			// Skip params that were handled above.
			continue
		}
		locals[name] = offset
		offset += WORD_SIZE
	}

	for _, op := range f.Ops {
		err := generateOp(output, op, locals)
		if err != nil {
			return nil
		}
	}

	fmt.Fprintf(output, "\n")
	fmt.Fprintf(output, "  // epilogue\n")
	// Restore x19-x22.
	fmt.Fprintf(output, "  ldp x19, x20, [sp, 16]\n")
	fmt.Fprintf(output, "  ldp x21, x22, [sp, 32]\n")

	// Restore FP and LR.
	fmt.Fprintf(output, "  ldp x29, x30, [sp], %d\n", allocWords*WORD_SIZE)

	fmt.Fprintf(output, "  ret\n")
	return nil
}

func generateOp(output io.Writer, op ir.Op, locals map[string]int) error {
	return nil
}
