package amd64_darwin

import (
	_ "embed"
	"fmt"
	"io"

	"github.com/iley/pirx/internal/parser"
)

const (
	WORD_SIZE = 8
)

//go:embed prologue.txt
var prologue string

type CodegenVisitor struct {
	output io.Writer
}

func NewCodegenVisitor(output io.Writer) *CodegenVisitor {
	return &CodegenVisitor{output: output}
}

var _ parser.AstVisitor = &CodegenVisitor{}

func (v *CodegenVisitor) printf(format string, a ...any) {
	fmt.Fprintf(v.output, format, a...)
}

func (v *CodegenVisitor) VisitProgram(p *parser.Program) {
	io.WriteString(v.output, prologue)
	for _, f := range p.Functions {
		v.VisitFunction(f)
	}
}

func (v *CodegenVisitor) VisitFunction(f *parser.Function) {
	io.WriteString(v.output, fmt.Sprintf("%s:\n", f.Name))

	nDeclarations := 0
	for _, s := range f.Body.Statements {
		if _, ok := s.(*parser.VariableDeclaration); ok {
			nDeclarations++
		}
	}

	// Allocate space on the stack:
	// * 2 words for FP and LR
	// * 4 words for x19-x22
	// * Function arguments.
	// * Local variables.
	allocWords := 2 + 4 + len(f.Params) + nDeclarations

	// For now we're going to assume that all variables are 64-bit.
	v.printf("  // prologue\n")
	v.printf("  stp x29, x30, [sp, -%d]!\n", allocWords*WORD_SIZE)
	v.printf("  mov x29, sp\n")
	v.printf("  stp x19, x20, [sp, 16]\n")
	v.printf("  stp x21, x22, [sp, 32]\n")

	// Arguments start at sp + 48.
	paramsStart := 48
	for i, _ := range f.Params {
		v.printf("  str x%d, [sp, %d]\n", i, paramsStart+(i*WORD_SIZE))
	}

	// Store offsets of local variables in a map.
	localsStart := paramsStart + len(f.Params)*WORD_SIZE
	localsMap := make(map[string]int)
	for i, decl := range f.Body.Statements {
		if decl, ok := decl.(*parser.VariableDeclaration); ok {
			localsMap[decl.Name] = localsStart + i*WORD_SIZE
		}
	}

	// TODO: Loop through the statemenets and generate code for them.

	v.printf("  // epilogue\n")
	// Restore x19-x22.
	v.printf("  ldp x19, x20, [sp, 16]\n")
	v.printf("  ldp x21, x22, [sp, 32]\n")

	// Restore FP and LR.
	v.printf("  ldp x29, x30, [sp], %d\n", allocWords*WORD_SIZE)

	v.printf("  ret\n")
}

func (v *CodegenVisitor) VisitParam(p *parser.Param)                             {}
func (v *CodegenVisitor) VisitBlock(b *parser.Block)                             {}
func (v *CodegenVisitor) VisitStatement(s parser.Statement)                      {}
func (v *CodegenVisitor) VisitLiteral(l *parser.Literal)                         {}
func (v *CodegenVisitor) VisitVariableDeclaration(d *parser.VariableDeclaration) {}
func (v *CodegenVisitor) VisitFunctionCall(f *parser.FunctionCall)               {}
func (v *CodegenVisitor) VisitExpressionStatement(e *parser.ExpressionStatement) {}
func (v *CodegenVisitor) VisitAssignment(a *parser.Assignment)                   {}
