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

type Statement struct {
	VariableDeclaration *VariableDeclaration
	ExpressionStatement *ExpressionStatement
	ReturnStatement     *ReturnStatement
	IfStatement         *IfStatement
	WhileStatement      *WhileStatement
	BreakStatement      *BreakStatement
	ContinueStatement   *ContinueStatement
}

func (s *Statement) String() string {
	if s.VariableDeclaration != nil {
		return s.VariableDeclaration.String()
	} else if s.ExpressionStatement != nil {
		return s.ExpressionStatement.String()
	} else if s.ReturnStatement != nil {
		return s.ReturnStatement.String()
	} else if s.IfStatement != nil {
		return s.IfStatement.String()
	} else if s.WhileStatement != nil {
		return s.WhileStatement.String()
	} else if s.BreakStatement != nil {
		return s.BreakStatement.String()
	} else if s.ContinueStatement != nil {
		return s.ContinueStatement.String()
	}
	panic(fmt.Sprintf("unsupported statement type: %v", *s))
}

type Expression struct {
	Literal           *Literal
	Assignment        *Assignment
	FunctionCall      *FunctionCall
	VariableReference *VariableReference
	BinaryOperation   *BinaryOperation
	UnaryOperation    *UnaryOperation
}

func (e *Expression) String() string {
	if e.Literal != nil {
		return e.Literal.String()
	} else if e.Assignment != nil {
		return e.Assignment.String()
	} else if e.FunctionCall != nil {
		return e.FunctionCall.String()
	} else if e.VariableReference != nil {
		return e.VariableReference.String()
	} else if e.BinaryOperation != nil {
		return e.BinaryOperation.String()
	} else if e.UnaryOperation != nil {
		return e.UnaryOperation.String()
	}
	panic("Invalid expression type")
}

type Literal struct {
	StringValue *string
	IntValue    *int64
}

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

type VariableDeclaration struct {
	Name string
	Type string
}

func (d *VariableDeclaration) String() string {
	return fmt.Sprintf("(decl %s %s)", d.Name, d.Type)
}

type FunctionCall struct {
	FunctionName string
	Args         []Expression
	Variadic     bool
}

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

type ExpressionStatement struct {
	Expression Expression
}

func (s *ExpressionStatement) String() string {
	return s.Expression.String()
}

type Assignment struct {
	VariableName string
	Value        Expression
}

func (a *Assignment) String() string {
	return fmt.Sprintf("(= %s %s)", a.VariableName, a.Value.String())
}

type VariableReference struct {
	Name string
}

func (v *VariableReference) String() string {
	return v.Name
}

type ReturnStatement struct {
	Value *Expression // optional return value
}

func (r *ReturnStatement) String() string {
	if r.Value == nil {
		return "(return)"
	} else {
		return fmt.Sprintf("(return %s)", r.Value.String())
	}
}

type BinaryOperation struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (b *BinaryOperation) String() string {
	return fmt.Sprintf("(%s %s %s)", b.Operator, b.Left.String(), b.Right.String())
}

type UnaryOperation struct {
	Operator string
	Operand  Expression
}

func (u *UnaryOperation) String() string {
	return fmt.Sprintf("(%s %s)", u.Operator, u.Operand.String())
}

type IfStatement struct {
	Condition Expression
	ThenBlock Block
	ElseBlock *Block // optional
}

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

func (w *WhileStatement) String() string {
	return fmt.Sprintf("(while %s %s)", w.Condition.String(), w.Body.String())
}

type BreakStatement struct {
}

func (b *BreakStatement) String() string {
	return "(break)"
}

type ContinueStatement struct {
}

func (c *ContinueStatement) String() string {
	return "(continue)"
}
