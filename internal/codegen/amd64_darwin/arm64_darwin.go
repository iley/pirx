package amd64_darwin

import (
	_ "embed"
	"fmt"
	"io"

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

	// TODO: Count variables, allocate memory for them, store offsets in a map.
	nDeclarations := 0

	// Allocate space on the stack:
	// * 2 words for FP and LR
	// * 4 words for x19-x22
	// * Function arguments.
	// * Local variables.
	allocWords := 2 + 4 + len(f.Params) + nDeclarations

	// For now we're going to assume that all variables are 64-bit.
	fmt.Fprintf(output, "  // prologue\n")
	fmt.Fprintf(output, "  stp x29, x30, [sp, -%d]!\n", allocWords*WORD_SIZE)
	fmt.Fprintf(output, "  mov x29, sp\n")
	fmt.Fprintf(output, "  stp x19, x20, [sp, 16]\n")
	fmt.Fprintf(output, "  stp x21, x22, [sp, 32]\n")

	// Arguments start at sp + 48.
	paramsStart := 48
	for i, _ := range f.Params {
		fmt.Fprintf(output, "  str x%d, [sp, %d]\n", i, paramsStart+(i*WORD_SIZE))
	}

	// TODO: Loop through the statemenets and generate code for them.

	fmt.Fprintf(output, "  // epilogue\n")
	// Restore x19-x22.
	fmt.Fprintf(output, "  ldp x19, x20, [sp, 16]\n")
	fmt.Fprintf(output, "  ldp x21, x22, [sp, 32]\n")

	// Restore FP and LR.
	fmt.Fprintf(output, "  ldp x29, x30, [sp], %d\n", allocWords*WORD_SIZE)

	fmt.Fprintf(output, "  ret\n")
	return nil
}
