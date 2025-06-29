package parser

import (
	"fmt"
	"strings"

	"github.com/iley/pirx/internal/util"
)

type AstNode interface {
	fmt.Stringer
}

type Program struct {
	Functions []*Function
}

func (p *Program) String() string {
	var sb strings.Builder
	sb.WriteString("(program")
	for _, fn := range p.Functions {
		sb.WriteString(" ")
		sb.WriteString(fn.String())
	}
	sb.WriteString(")")
	return sb.String()
}

type Function struct {
	Name   string
	Params []Param
	Body   Block
}

func (f *Function) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("(func %s (", f.Name))
	for i, param := range f.Params {
		sb.WriteString(param.String())
		if i != len(f.Params)-1 {
			sb.WriteString(" ")
		}
	}
	sb.WriteString(") ")
	sb.WriteString(f.Body.String())
	sb.WriteString(")")
	return sb.String()
}

type Param struct {
	Name string
	Type string
}

func (p Param) String() string {
	return fmt.Sprintf("(%s %s)", p.Name, p.Type)
}

type Block struct {
	Statements []Statement
}

func (b *Block) String() string {
	var sb strings.Builder
	sb.WriteString("(block")
	for _, stmt := range b.Statements {
		sb.WriteString(" ")
		sb.WriteString(stmt.String())
	}
	sb.WriteString(")")
	return sb.String()
}

// Statement types.

type Statement interface {
	AstNode
	isStatement()
}

type VariableDeclaration struct {
	Name string
	Type string
}

func (d *VariableDeclaration) isStatement() {}

func (d *VariableDeclaration) String() string {
	return fmt.Sprintf("(decl %s %s)", d.Name, d.Type)
}

type ExpressionStatement struct {
	Expression Expression
}

func (s *ExpressionStatement) isStatement() {}

func (s *ExpressionStatement) String() string {
	return s.Expression.String()
}

type ReturnStatement struct {
	Value Expression // optional return value
}

func (r *ReturnStatement) isStatement() {}

func (r *ReturnStatement) String() string {
	if r.Value == nil {
		return "(return)"
	} else {
		return fmt.Sprintf("(return %s)", r.Value.String())
	}
}

type IfStatement struct {
	Condition Expression
	ThenBlock Block
	ElseBlock *Block // optional
}

func (i *IfStatement) isStatement() {}

func (i *IfStatement) String() string {
	if i.ElseBlock == nil {
		return fmt.Sprintf("(if %s %s)", i.Condition.String(), i.ThenBlock.String())
	} else {
		return fmt.Sprintf("(if %s %s %s)", i.Condition.String(), i.ThenBlock.String(), i.ElseBlock.String())
	}
}

type WhileStatement struct {
	Condition Expression
	Body      Block
}

func (w *WhileStatement) isStatement() {}

func (w *WhileStatement) String() string {
	return fmt.Sprintf("(while %s %s)", w.Condition.String(), w.Body.String())
}

type BreakStatement struct {
}

func (b *BreakStatement) isStatement() {}

func (b *BreakStatement) String() string {
	return "(break)"
}

type ContinueStatement struct {
}

func (c *ContinueStatement) isStatement() {}

func (c *ContinueStatement) String() string {
	return "(continue)"
}

// Expression types.

type Expression interface {
	AstNode
	isExpression()
}

type Literal struct {
	StringValue *string
	IntValue    *int64
}

func (l *Literal) isExpression() {}

func (l *Literal) String() string {
	if l.StringValue != nil {
		return fmt.Sprintf("\"%s\"", util.EscapeString(*l.StringValue))
	} else if l.IntValue != nil {
		return fmt.Sprintf("%d", *l.IntValue)
	}
	panic(fmt.Sprintf("unknown literal type: %v", *l))
}

// Helper functions for creating Literal values
func NewIntLiteral(value int64) *Literal {
	return &Literal{IntValue: &value}
}

func NewStringLiteral(value string) *Literal {
	return &Literal{StringValue: &value}
}

type Assignment struct {
	VariableName string
	Value        Expression
}

func (a *Assignment) isExpression() {}

func (a *Assignment) String() string {
	return fmt.Sprintf("(= %s %s)", a.VariableName, a.Value.String())
}

type VariableReference struct {
	Name string
}

func (v *VariableReference) isExpression() {}

func (v *VariableReference) String() string {
	return v.Name
}

type FunctionCall struct {
	FunctionName string
	Args         []Expression
	Variadic     bool
}

func (f *FunctionCall) isExpression() {}

func (f *FunctionCall) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("(%s", f.FunctionName))
	for _, arg := range f.Args {
		sb.WriteString(" ")
		sb.WriteString(arg.String())
	}
	sb.WriteString(")")
	return sb.String()
}

type BinaryOperation struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (b *BinaryOperation) isExpression() {}

func (b *BinaryOperation) String() string {
	return fmt.Sprintf("(%s %s %s)", b.Operator, b.Left.String(), b.Right.String())
}

type UnaryOperation struct {
	Operator string
	Operand  Expression
}

func (u *UnaryOperation) isExpression() {}

func (u *UnaryOperation) String() string {
	return fmt.Sprintf("(%s %s)", u.Operator, u.Operand.String())
}
