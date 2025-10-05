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
	GetType() Type
}

type Program struct {
	Loc                  Location
	Functions            []Function
	TypeDeclarations     []TypeDeclaration
	VariableDeclarations []VariableDeclaration
	TypeTable            *TypeTable
}

func (p *Program) GetLocation() Location {
	return p.Loc
}

func (p *Program) GetType() Type {
	return nil
}

func (p *Program) String() string {
	var sb strings.Builder
	sb.WriteString("(program")
	for _, decl := range p.TypeDeclarations {
		sb.WriteString(" ")
		sb.WriteString(decl.String())
	}
	for _, varDecl := range p.VariableDeclarations {
		sb.WriteString(" ")
		sb.WriteString(varDecl.String())
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
	Body       *Block
	ReturnType Type
	External   bool
}

func (f *Function) GetLocation() Location {
	return f.Loc
}

func (f *Function) GetType() Type {
	return nil
}

func (f *Function) String() string {
	var sb strings.Builder
	if f.External {
		sb.WriteString(fmt.Sprintf("(extern func %s (", f.Name))
	} else {
		sb.WriteString(fmt.Sprintf("(func %s (", f.Name))
	}
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
	if f.Body != nil {
		sb.WriteString(" ")
		sb.WriteString(f.Body.String())
	}
	sb.WriteString(")")
	return sb.String()
}

type TypeDeclaration interface {
	fmt.Stringer
	GetTypeName() string
	GetLocation() Location
}

type StructDeclaration struct {
	Loc    Location
	Name   string
	Fields []StructField
}

func (s *StructDeclaration) GetTypeName() string {
	return s.Name
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

func (f StructField) GetType() Type {
	return f.Type
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

func (a Arg) GetType() Type {
	return a.Type
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

func (b *Block) GetType() Type {
	return nil
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
	Loc         Location
	Name        string
	Type        Type       // Optional type annotation (nil for type inference)
	Initializer Expression // Optional initializer expression
	IsConstant  bool       // true if this is a constant declaration (val), false for variable (var)
}

func (d *VariableDeclaration) GetLocation() Location {
	return d.Loc
}

func (d *VariableDeclaration) GetType() Type {
	return nil
}

func (d *VariableDeclaration) isStatement() {}

func (d *VariableDeclaration) String() string {
	typeStr := ""
	if d.Type != nil {
		typeStr = d.Type.String()
	} else {
		typeStr = "inferred"
	}

	keyword := "decl"
	if d.IsConstant {
		keyword = "val"
	}

	if d.Initializer != nil {
		return fmt.Sprintf("(%s %s %s %s)", keyword, d.Name, typeStr, d.Initializer.String())
	}
	return fmt.Sprintf("(%s %s %s)", keyword, d.Name, typeStr)
}

type ExpressionStatement struct {
	Loc        Location
	Expression Expression
}

func (s *ExpressionStatement) GetLocation() Location {
	return s.Loc
}

func (s *ExpressionStatement) GetType() Type {
	return nil
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

func (r *ReturnStatement) GetType() Type {
	return nil
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

func (i *IfStatement) GetType() Type {
	return nil
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

func (w *WhileStatement) GetType() Type {
	return nil
}

func (w *WhileStatement) isStatement() {}

func (w *WhileStatement) String() string {
	return fmt.Sprintf("(while %s %s)", w.Condition.String(), w.Body.String())
}

type ForStatement struct {
	Loc       Location
	Init      Statement  // initialization statement (e.g., var i = 0)
	Condition Expression // loop condition (e.g., i < 10)
	Update    Expression // update expression (e.g., i++)
	Body      Block      // loop body
}

func (f *ForStatement) GetLocation() Location {
	return f.Loc
}

func (f *ForStatement) GetType() Type {
	return nil
}

func (f *ForStatement) isStatement() {}

func (f *ForStatement) String() string {
	initStr := ""
	if f.Init != nil {
		initStr = f.Init.String()
	}
	condStr := ""
	if f.Condition != nil {
		condStr = f.Condition.String()
	}
	updateStr := ""
	if f.Update != nil {
		updateStr = f.Update.String()
	}
	return fmt.Sprintf("(for %s %s %s %s)", initStr, condStr, updateStr, f.Body.String())
}

type BreakStatement struct {
	Loc Location
}

func (b *BreakStatement) GetLocation() Location {
	return b.Loc
}

func (b *BreakStatement) GetType() Type {
	return nil
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

func (c *ContinueStatement) GetType() Type {
	return nil
}

func (c *ContinueStatement) isStatement() {}

func (c *ContinueStatement) String() string {
	return "(continue)"
}

type BlockStatement struct {
	Loc   Location
	Block Block
}

func (b *BlockStatement) GetLocation() Location {
	return b.Loc
}

func (b *BlockStatement) GetType() Type {
	return nil
}

func (b *BlockStatement) isStatement() {}

func (b *BlockStatement) String() string {
	return b.Block.String()
}

// Expression types.

type Expression interface {
	AstNode
	isExpression()
}

type Literal struct {
	Loc          Location
	StringValue  *string
	IntValue     *int32
	Int8Value    *int8
	Int64Value   *int64
	Float32Value *float32
	Float64Value *float64
	BoolValue    *bool
	NullValue    bool // true if this is a null literal
	Type         Type
}

func (l *Literal) GetLocation() Location {
	return l.Loc
}

func (l *Literal) GetType() Type {
	return l.Type
}

func (l *Literal) isExpression() {}

func (l *Literal) String() string {
	if l.StringValue != nil {
		return fmt.Sprintf("\"%s\"", util.EscapeString(*l.StringValue))
	} else if l.IntValue != nil {
		return fmt.Sprintf("%d", *l.IntValue)
	} else if l.Int8Value != nil {
		return fmt.Sprintf("%d", *l.Int8Value)
	} else if l.Int64Value != nil {
		return fmt.Sprintf("%d", *l.Int64Value)
	} else if l.Float32Value != nil {
		return fmt.Sprintf("%g", *l.Float32Value)
	} else if l.Float64Value != nil {
		return fmt.Sprintf("%g", *l.Float64Value)
	} else if l.BoolValue != nil {
		return fmt.Sprintf("%v", *l.BoolValue)
	} else if l.NullValue {
		return "null"
	}
	panic(fmt.Sprintf("unknown literal type: %v", *l))
}

// Helper functions for creating Literal values
func NewIntLiteral(value int32) *Literal {
	return &Literal{IntValue: &value}
}

func NewInt8Literal(value int8) *Literal {
	return &Literal{Int8Value: &value}
}

func NewInt64Literal(value int64) *Literal {
	return &Literal{Int64Value: &value}
}

func NewFloat32Literal(value float32) *Literal {
	return &Literal{Float32Value: &value}
}

func NewFloat64Literal(value float64) *Literal {
	return &Literal{Float64Value: &value}
}

func NewStringLiteral(value string) *Literal {
	return &Literal{StringValue: &value}
}

func NewBoolLiteral(value bool) *Literal {
	return &Literal{BoolValue: &value}
}

func NewNullLiteral() *Literal {
	return &Literal{NullValue: true}
}

type Assignment struct {
	Loc      Location
	Target   Expression
	Value    Expression
	Type     Type
	Operator string // "=", "+=", or "-="
}

func (a *Assignment) GetLocation() Location {
	return a.Loc
}

func (a *Assignment) GetType() Type {
	return a.Type
}

func (a *Assignment) isExpression() {}

func (a *Assignment) String() string {
	op := a.Operator
	if op == "" {
		op = "="
	}
	return fmt.Sprintf("(%s %s %s)", op, a.Target.String(), a.Value.String())
}

type VariableReference struct {
	Loc  Location
	Name string
	Type Type
}

func (v *VariableReference) GetLocation() Location {
	return v.Loc
}

func (v *VariableReference) GetType() Type {
	return v.Type
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
	Type         Type
}

func (f *FunctionCall) GetLocation() Location {
	return f.Loc
}

func (f *FunctionCall) GetType() Type {
	return f.Type
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
	Type     Type
}

func (b *BinaryOperation) GetLocation() Location {
	return b.Loc
}

func (b *BinaryOperation) GetType() Type {
	return b.Type
}

func (b *BinaryOperation) isExpression() {}

func (b *BinaryOperation) String() string {
	return fmt.Sprintf("(%s %s %s)", b.Operator, b.Left.String(), b.Right.String())
}

type UnaryOperation struct {
	Loc      Location
	Operator string
	Operand  Expression
	Type     Type
}

func (u *UnaryOperation) GetLocation() Location {
	return u.Loc
}

func (u *UnaryOperation) GetType() Type {
	return u.Type
}

func (u *UnaryOperation) isExpression() {}

func (u *UnaryOperation) String() string {
	return fmt.Sprintf("(%s %s)", u.Operator, u.Operand.String())
}

type FieldAccess struct {
	Loc       Location
	Object    Expression
	FieldName string
	Type      Type
}

func (f *FieldAccess) GetLocation() Location {
	return f.Loc
}

func (f *FieldAccess) GetType() Type {
	return f.Type
}

func (f *FieldAccess) isExpression() {}

func (f *FieldAccess) String() string {
	return fmt.Sprintf("(. %s %s)", f.Object.String(), f.FieldName)
}

type IndexExpression struct {
	Loc   Location
	Array Expression
	Index Expression
	Type  Type
}

func (i *IndexExpression) GetLocation() Location {
	return i.Loc
}

func (i *IndexExpression) GetType() Type {
	return i.Type
}

func (i *IndexExpression) isExpression() {}

func (i *IndexExpression) String() string {
	return fmt.Sprintf("([] %s %s)", i.Array.String(), i.Index.String())
}

type RangeExpression struct {
	Loc   Location
	Array Expression
	Start Expression
	End   Expression
	Type  Type
}

func (r *RangeExpression) GetLocation() Location {
	return r.Loc
}

func (r *RangeExpression) GetType() Type {
	return r.Type
}

func (r *RangeExpression) isExpression() {}

func (r *RangeExpression) String() string {
	return fmt.Sprintf("([:] %s %s %s)", r.Array.String(), r.Start.String(), r.End.String())
}

type NewExpression struct {
	Loc      Location
	TypeExpr Type
	Count    Expression // Optional count for slice allocation
	Type     Type
}

func (n *NewExpression) GetLocation() Location {
	return n.Loc
}

func (n *NewExpression) GetType() Type {
	return n.Type
}

func (n *NewExpression) isExpression() {}

func (n *NewExpression) String() string {
	if n.Count != nil {
		return fmt.Sprintf("(new %s %s)", n.TypeExpr.String(), n.Count.String())
	}
	return fmt.Sprintf("(new %s)", n.TypeExpr.String())
}

type PostfixOperator struct {
	Loc      Location
	Operator string
	Operand  Expression
	Type     Type
}

func (p *PostfixOperator) GetLocation() Location {
	return p.Loc
}

func (p *PostfixOperator) GetType() Type {
	return p.Type
}

func (p *PostfixOperator) isExpression() {}

func (p *PostfixOperator) String() string {
	return fmt.Sprintf("(%s %s)", p.Operator, p.Operand.String())
}
