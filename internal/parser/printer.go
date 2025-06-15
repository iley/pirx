package parser

import (
	"fmt"
	"io"
	"strings"
)

type Printer struct {
	output      io.Writer
	indentLevel int
}

func NewPrinter(output io.Writer) *Printer {
	return &Printer{output: output}
}

func (p *Printer) write(line string) {
	fmt.Fprint(p.output, line)
}

func (p *Printer) writeln(line string) {
	p.write(line)
	p.write("\n")
}

func (p *Printer) indent() {
	p.indentLevel++
}

func (p *Printer) dedent() {
	p.indentLevel--
}

func (p *Printer) writeIndent() {
	p.write(strings.Repeat("  ", p.indentLevel))
}

func (p *Printer) VisitProgram(program *Program) {
	for i, fn := range program.Functions {
		fn.Accept(p)
		if i < len(program.Functions)-1 {
			p.writeln("")
		}
	}
}

func (p *Printer) VisitFunction(function *Function) {
	// Print function declaration
	p.write("func " + function.Name + "(")

	// Print parameters
	params := make([]string, len(function.Params))
	for i, param := range function.Params {
		params[i] = param.Name + " " + param.Type
	}
	p.write(strings.Join(params, ", "))
	p.writeln(") {")

	// Print function body
	p.indent()
	function.Body.Accept(p)
	p.dedent()
	p.writeln("}")
}

func (p *Printer) VisitParam(param *Param) {
	// Handled in VisitFunction
}

func (p *Printer) VisitBlock(block *Block) {
	for _, stmt := range block.Statements {
		p.writeIndent()
		stmt.Accept(p)
		p.writeln("")
	}
}

func (p *Printer) VisitStatement(statement Statement) {
	statement.Accept(p)
}

func (p *Printer) VisitLiteral(literal *Literal) {
	switch literal.Type {
	case LiteralTypeString:
		p.write(fmt.Sprintf("%q", literal.StringValue))
	case LiteralTypeInt:
		p.write(fmt.Sprintf("%d", literal.IntValue))
	}
}

func (p *Printer) VisitVariableDeclaration(vd *VariableDeclaration) {
	p.write("var " + vd.Name)
	if vd.Type != "" {
		p.write(" " + vd.Type)
	}
}

func (p *Printer) VisitFunctionCall(fc *FunctionCall) {
	p.write(fc.FunctionName + "(")

	args := make([]string, len(fc.Args))
	for i, arg := range fc.Args {
		// Create a temporary printer to capture the argument's string representation
		var argStr strings.Builder
		argPrinter := NewPrinter(&argStr)
		arg.Accept(argPrinter)
		args[i] = argStr.String()
	}

	p.write(strings.Join(args, ", "))
	p.write(")")
}

func (p *Printer) VisitExpressionStatement(s *ExpressionStatement) {
	s.Expression.Accept(p)
	p.writeln("")
}

func (p *Printer) VisitAssignment(assignment *Assignment) {
	p.write(assignment.VariableName + " = ")
	assignment.Value.Accept(p)
}
