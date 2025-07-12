package ast

import (
	"fmt"
	"strings"

	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/util"
)

type Location = lexer.Location

type AstNode interface {
	fmt.Stringer
	GetLocation() Location
}

type Program struct {
	Loc                Location
	Functions          []Function
	ExternFunctions    []ExternFunction
	StructDeclarations []StructDeclaration
}

func (p *Program) GetLocation() Location {
	return p.Loc
}

func (p *Program) String() string {
	var sb strings.Builder
	sb.WriteString("(program")
	for _, structDecl := range p.StructDeclarations {
		sb.WriteString(" ")
		sb.WriteString(structDecl.String())
	}
	for _, fn := range p.ExternFunctions {
		sb.WriteString(" ")
		sb.WriteString(fn.String())
	}
	for _, fn := range p.Functions {
		sb.WriteString(" ")
		sb.WriteString(fn.String())
	}
	sb.WriteString(")")
	return sb.String()
}

type Function struct {
	Loc        Location
	Name       string
	Args       []Arg
	Body       Block
	ReturnType Type
}

func (f *Function) GetLocation() Location {
	return f.Loc
}

func (f *Function) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("(func %s (", f.Name))
	for i, arg := range f.Args {
		sb.WriteString(arg.String())
		if i != len(f.Args)-1 {
			sb.WriteString(" ")
		}
	}
	sb.WriteString(") ")
	if f.ReturnType == nil {
		sb.WriteString("() ")
	} else {
		sb.WriteString(f.ReturnType.String() + " ")
	}
	sb.WriteString(f.Body.String())
	sb.WriteString(")")
	return sb.String()
}

type ExternFunction struct {
	Loc        Location
	Name       string
	Args       []Arg
	ReturnType Type
}

func (f *ExternFunction) GetLocation() Location {
	return f.Loc
}

func (f *ExternFunction) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("(extern func %s (", f.Name))
	for i, arg := range f.Args {
		sb.WriteString(arg.String())
		if i != len(f.Args)-1 {
			sb.WriteString(" ")
		}
	}
	sb.WriteString(")")
	if f.ReturnType != nil {
		sb.WriteString(": " + f.ReturnType.String())
	}
	sb.WriteString(")")
	return sb.String()
}

type StructDeclaration struct {
	Loc    Location
	Name   string
	Fields []StructField
}

func (s *StructDeclaration) GetLocation() Location {
	return s.Loc
}

func (s *StructDeclaration) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("(struct %s (", s.Name))
	for i, field := range s.Fields {
		sb.WriteString(field.String())
		if i != len(s.Fields)-1 {
			sb.WriteString(" ")
		}
	}
	sb.WriteString("))")
	return sb.String()
}

type StructField struct {
	Loc  Location
	Name string
	Type Type
}

func (f StructField) GetLocation() Location {
	return f.Loc
}

func (f StructField) String() string {
	return fmt.Sprintf("(%s %s)", f.Name, f.Type.String())
}

type Arg struct {
	Loc  Location
	Name string
	Type Type
}

func (a Arg) GetLocation() Location {
	return a.Loc
}

func (a Arg) String() string {
	return fmt.Sprintf("(%s %s)", a.Name, a.Type.String())
}

type Block struct {
	Loc        Location
	Statements []Statement
}

func (b *Block) GetLocation() Location {
	return b.Loc
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
	Loc  Location
	Name string
	Type Type
}

func (d *VariableDeclaration) GetLocation() Location {
	return d.Loc
}

func (d *VariableDeclaration) isStatement() {}

func (d *VariableDeclaration) String() string {
	return fmt.Sprintf("(decl %s %s)", d.Name, d.Type.String())
}

type ExpressionStatement struct {
	Loc        Location
	Expression Expression
}

func (s *ExpressionStatement) GetLocation() Location {
	return s.Loc
}

func (s *ExpressionStatement) isStatement() {}

func (s *ExpressionStatement) String() string {
	return s.Expression.String()
}

type ReturnStatement struct {
	Loc   Location
	Value Expression // optional return value
}

func (r *ReturnStatement) GetLocation() Location {
	return r.Loc
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
	Loc       Location
	Condition Expression
	ThenBlock Block
	ElseBlock *Block // optional
}

func (i *IfStatement) GetLocation() Location {
	return i.Loc
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
	Loc       Location
	Condition Expression
	Body      Block
}

func (w *WhileStatement) GetLocation() Location {
	return w.Loc
}

func (w *WhileStatement) isStatement() {}

func (w *WhileStatement) String() string {
	return fmt.Sprintf("(while %s %s)", w.Condition.String(), w.Body.String())
}

type BreakStatement struct {
	Loc Location
}

func (b *BreakStatement) GetLocation() Location {
	return b.Loc
}

func (b *BreakStatement) isStatement() {}

func (b *BreakStatement) String() string {
	return "(break)"
}

type ContinueStatement struct {
	Loc Location
}

func (c *ContinueStatement) GetLocation() Location {
	return c.Loc
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

// LValue types for assignment targets
type LValue interface {
	AstNode
	isLValue()
}

type VariableLValue struct {
	Loc  Location
	Name string
}

func (v *VariableLValue) GetLocation() Location {
	return v.Loc
}

func (v *VariableLValue) isLValue() {}

func (v *VariableLValue) String() string {
	return v.Name
}

type DereferenceLValue struct {
	Loc        Location
	Expression Expression
}

func (d *DereferenceLValue) GetLocation() Location {
	return d.Loc
}

func (d *DereferenceLValue) isLValue() {}

func (d *DereferenceLValue) String() string {
	return fmt.Sprintf("(* %s)", d.Expression.String())
}

type FieldLValue struct {
	Loc       Location
	Object    Expression
	FieldName string
}

func (f *FieldLValue) GetLocation() Location {
	return f.Loc
}

func (f *FieldLValue) isLValue() {}

func (f *FieldLValue) String() string {
	return fmt.Sprintf("(. %s %s)", f.Object.String(), f.FieldName)
}

type Literal struct {
	Loc         Location
	StringValue *string
	IntValue    *int32
	Int64Value  *int64
	BoolValue   *bool
}

func (l *Literal) GetLocation() Location {
	return l.Loc
}

func (l *Literal) isExpression() {}

func (l *Literal) String() string {
	if l.StringValue != nil {
		return fmt.Sprintf("\"%s\"", util.EscapeString(*l.StringValue))
	} else if l.IntValue != nil {
		return fmt.Sprintf("%d", *l.IntValue)
	} else if l.Int64Value != nil {
		return fmt.Sprintf("%d", *l.Int64Value)
	} else if l.BoolValue != nil {
		return fmt.Sprintf("%v", *l.BoolValue)
	}
	panic(fmt.Sprintf("unknown literal type: %v", *l))
}

// Helper functions for creating Literal values
func NewIntLiteral(value int32) *Literal {
	return &Literal{IntValue: &value}
}

func NewInt64Literal(value int64) *Literal {
	return &Literal{Int64Value: &value}
}

func NewStringLiteral(value string) *Literal {
	return &Literal{StringValue: &value}
}

func NewBoolLiteral(value bool) *Literal {
	return &Literal{BoolValue: &value}
}

type Assignment struct {
	Loc    Location
	Target LValue
	Value  Expression
}

func (a *Assignment) GetLocation() Location {
	return a.Loc
}

func (a *Assignment) isExpression() {}

func (a *Assignment) String() string {
	return fmt.Sprintf("(= %s %s)", a.Target.String(), a.Value.String())
}

type VariableReference struct {
	Loc  Location
	Name string
}

func (v *VariableReference) GetLocation() Location {
	return v.Loc
}

func (v *VariableReference) isExpression() {}

func (v *VariableReference) String() string {
	return v.Name
}

type FunctionCall struct {
	Loc          Location
	FunctionName string
	Args         []Expression
	Variadic     bool
}

func (f *FunctionCall) GetLocation() Location {
	return f.Loc
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
	Loc      Location
	Left     Expression
	Operator string
	Right    Expression
}

func (b *BinaryOperation) GetLocation() Location {
	return b.Loc
}

func (b *BinaryOperation) isExpression() {}

func (b *BinaryOperation) String() string {
	return fmt.Sprintf("(%s %s %s)", b.Operator, b.Left.String(), b.Right.String())
}

type UnaryOperation struct {
	Loc      Location
	Operator string
	Operand  Expression
}

func (u *UnaryOperation) GetLocation() Location {
	return u.Loc
}

func (u *UnaryOperation) isExpression() {}

func (u *UnaryOperation) String() string {
	return fmt.Sprintf("(%s %s)", u.Operator, u.Operand.String())
}

type FieldAccess struct {
	Loc       Location
	Object    Expression
	FieldName string
}

func (f *FieldAccess) GetLocation() Location {
	return f.Loc
}

func (f *FieldAccess) isExpression() {}

func (f *FieldAccess) String() string {
	return fmt.Sprintf("(. %s %s)", f.Object.String(), f.FieldName)
}
