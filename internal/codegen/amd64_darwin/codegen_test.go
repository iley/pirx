package amd64_darwin

import (
	"bytes"
	"strings"
	"testing"

	"github.com/iley/pirx/internal/parser"
)

func TestCodegenVisitor_EmptyProgram(t *testing.T) {
	// Create an empty program with just an empty main function
	program := &parser.Program{
		Functions: []*parser.Function{
			{
				Name:   "main",
				Params: []*parser.Param{},
				Body:   &parser.Block{Statements: []parser.Statement{}},
			},
		},
	}

	// Create a buffer to capture the output
	var output bytes.Buffer
	visitor := NewCodegenVisitor(&output)

	// Generate code
	visitor.VisitProgram(program)

	// Get the generated assembly
	result := output.String()

	// Check that the output contains expected parts
	expectedParts := []string{
		".global _start", // from prologue
		".align 4",       // from prologue
		"print:",         // from prologue
		"_start:",        // from prologue
		"bl main",        // from prologue
		"main:",          // function label
		"// prologue",    // function prologue comment
		"stp x29, x30",   // save frame pointer and link register
		"mov x29, sp",    // set up frame pointer
		"stp x19, x20",   // save callee-saved registers
		"stp x21, x22",   // save callee-saved registers
		"// epilogue",    // function epilogue comment
		"ldp x19, x20",   // restore callee-saved registers
		"ldp x21, x22",   // restore callee-saved registers
		"ldp x29, x30",   // restore frame pointer and link register
		"ret",            // return instruction
	}

	for _, expected := range expectedParts {
		if !strings.Contains(result, expected) {
			t.Errorf("Expected output to contain %q, but it was missing.\nFull output:\n%s", expected, result)
		}
	}

	// Check that the stack allocation is correct for empty function
	// Expected: 2 words (FP+LR) + 4 words (x19-x22) + 0 params + 0 locals = 6 words = 48 bytes
	expectedStackAlloc := "stp x29, x30, [sp, -48]!"
	if !strings.Contains(result, expectedStackAlloc) {
		t.Errorf("Expected stack allocation %q, but it was missing.\nFull output:\n%s", expectedStackAlloc, result)
	}

	// Check that the stack deallocation matches
	expectedStackDealloc := "ldp x29, x30, [sp], 48"
	if !strings.Contains(result, expectedStackDealloc) {
		t.Errorf("Expected stack deallocation %q, but it was missing.\nFull output:\n%s", expectedStackDealloc, result)
	}
}
