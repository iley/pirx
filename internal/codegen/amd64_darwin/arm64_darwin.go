package amd64_darwin

import (
	_ "embed"
	"fmt"
	"io"

	"github.com/iley/pirx/internal/parser"
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

func (v *CodegenVisitor) VisitProgram(p *parser.Program) {
	io.WriteString(v.output, prologue)
	for _, f := range p.Functions {
		v.VisitFunction(f)
	}
}

func (v *CodegenVisitor) VisitFunction(f *parser.Function) {
	io.WriteString(v.output, fmt.Sprintf("%s:\n", f.Name))

	io.WriteString(v.output, "  ret\n")
}

func (v *CodegenVisitor) VisitParam(p *parser.Param)                             {}
func (v *CodegenVisitor) VisitBlock(b *parser.Block)                             {}
func (v *CodegenVisitor) VisitStatement(s parser.Statement)                      {}
func (v *CodegenVisitor) VisitLiteral(l *parser.Literal)                         {}
func (v *CodegenVisitor) VisitVariableDeclaration(d *parser.VariableDeclaration) {}
func (v *CodegenVisitor) VisitFunctionCall(f *parser.FunctionCall)               {}
func (v *CodegenVisitor) VisitExpressionStatement(e *parser.ExpressionStatement) {}
