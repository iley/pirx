package amd64_darwin

import (
	"io"

	"github.com/iley/pirx/internal/parser"
)

type CodegenVisitor struct {
	output io.Writer
}

func NewCodegenVisitor(output io.Writer) *CodegenVisitor {
	return &CodegenVisitor{output: output}
}

var _ parser.AstVisitor = &CodegenVisitor{}

func (v *CodegenVisitor) VisitProgram(p *parser.Program) {
}

func (v *CodegenVisitor) VisitFunction(f *parser.Function)                       {}
func (v *CodegenVisitor) VisitParam(p *parser.Param)                             {}
func (v *CodegenVisitor) VisitBlock(b *parser.Block)                             {}
func (v *CodegenVisitor) VisitStatement(s parser.Statement)                      {}
func (v *CodegenVisitor) VisitLiteral(l *parser.Literal)                         {}
func (v *CodegenVisitor) VisitVariableDeclaration(d *parser.VariableDeclaration) {}
func (v *CodegenVisitor) VisitFunctionCall(f *parser.FunctionCall)               {}
func (v *CodegenVisitor) VisitExpressionStatement(e *parser.ExpressionStatement) {}
