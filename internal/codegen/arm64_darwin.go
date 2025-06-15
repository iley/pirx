package codegen

import (
	"io"

	"github.com/iley/pirx/internal/parser"
)

type ARM64DarwinCodegenVisitor struct {
	output io.Writer
}

func NewARM64DarwinCodegenVisitor(output io.Writer) *ARM64DarwinCodegenVisitor {
	return &ARM64DarwinCodegenVisitor{output: output}
}

var _ parser.AstVisitor = &ARM64DarwinCodegenVisitor{}

func (v *ARM64DarwinCodegenVisitor) VisitProgram(p *parser.Program)                         {}
func (v *ARM64DarwinCodegenVisitor) VisitFunction(f *parser.Function)                       {}
func (v *ARM64DarwinCodegenVisitor) VisitParam(p *parser.Param)                             {}
func (v *ARM64DarwinCodegenVisitor) VisitBlock(b *parser.Block)                             {}
func (v *ARM64DarwinCodegenVisitor) VisitStatement(s parser.Statement)                      {}
func (v *ARM64DarwinCodegenVisitor) VisitLiteral(l *parser.Literal)                         {}
func (v *ARM64DarwinCodegenVisitor) VisitVariableDeclaration(d *parser.VariableDeclaration) {}
func (v *ARM64DarwinCodegenVisitor) VisitFunctionCall(f *parser.FunctionCall)               {}
func (v *ARM64DarwinCodegenVisitor) VisitExpressionStatement(e *parser.ExpressionStatement) {}
