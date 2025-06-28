package parser

import "fmt"

type AstNode interface {
	Accept(visitor AstVisitor)
}

type Program struct {
	Functions []*Function
}

func (p *Program) Accept(visitor AstVisitor) {
	visitor.VisitProgram(p)
}

type Function struct {
	Name   string
	Params []*Param
	Body   *Block
}

func (f *Function) Accept(visitor AstVisitor) {
	visitor.VisitFunction(f)
}

type Param struct {
	Name string
	Type string
}

func (p *Param) Accept(visitor AstVisitor) {
	visitor.VisitParam(p)
}

type Block struct {
	Statements []Statement
}

func (b *Block) Accept(visitor AstVisitor) {
	visitor.VisitBlock(b)
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

func (s *Statement) Accept(visitor AstVisitor) {
	if s.VariableDeclaration != nil {
		s.VariableDeclaration.Accept(visitor)
	} else if s.ExpressionStatement != nil {
		s.ExpressionStatement.Accept(visitor)
	} else if s.ReturnStatement != nil {
		s.ReturnStatement.Accept(visitor)
	} else if s.IfStatement != nil {
		s.IfStatement.Accept(visitor)
	} else if s.WhileStatement != nil {
		s.WhileStatement.Accept(visitor)
	} else if s.BreakStatement != nil {
		s.BreakStatement.Accept(visitor)
	} else if s.ContinueStatement != nil {
		s.ContinueStatement.Accept(visitor)
	} else {
		panic(fmt.Sprintf("unsupported statement type: %v", *s))
	}
}

type Expression struct {
	Literal           *Literal
	Assignment        *Assignment
	FunctionCall      *FunctionCall
	VariableReference *VariableReference
	BinaryOperation   *BinaryOperation
	UnaryOperation    *UnaryOperation
}

func (e *Expression) Accept(visitor AstVisitor) {
	if e.Literal != nil {
		e.Literal.Accept(visitor)
	} else if e.Assignment != nil {
		e.Assignment.Accept(visitor)
	} else if e.FunctionCall != nil {
		e.FunctionCall.Accept(visitor)
	} else if e.VariableReference != nil {
		e.VariableReference.Accept(visitor)
	} else if e.BinaryOperation != nil {
		e.BinaryOperation.Accept(visitor)
	} else if e.UnaryOperation != nil {
		e.UnaryOperation.Accept(visitor)
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", e))
	}
}

type Literal struct {
	StringValue *string
	IntValue    *int64
}

func (l *Literal) Accept(visitor AstVisitor) {
	visitor.VisitLiteral(l)
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

func (v *VariableDeclaration) Accept(visitor AstVisitor) {
	visitor.VisitVariableDeclaration(v)
}

type FunctionCall struct {
	FunctionName string
	Args         []Expression
	Variadic     bool
}

func (f *FunctionCall) Accept(visitor AstVisitor) {
	visitor.VisitFunctionCall(f)
}

type ExpressionStatement struct {
	Expression Expression
}

func (s *ExpressionStatement) Accept(visitor AstVisitor) {
	visitor.VisitExpressionStatement(s)
}

type Assignment struct {
	VariableName string
	Value        Expression
}

func (a *Assignment) Accept(visitor AstVisitor) {
	visitor.VisitAssignment(a)
}

type VariableReference struct {
	Name string
}

func (v *VariableReference) Accept(visitor AstVisitor) {
	visitor.VisitVariableReference(v)
}

type ReturnStatement struct {
	Value *Expression // optional return value
}

func (r *ReturnStatement) Accept(visitor AstVisitor) {
	visitor.VisitReturnStatement(r)
}

type BinaryOperation struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (b *BinaryOperation) Accept(visitor AstVisitor) {
	visitor.VisitBinaryOperation(b)
}

type UnaryOperation struct {
	Operator string
	Operand  Expression
}

func (u *UnaryOperation) Accept(visitor AstVisitor) {
	visitor.VisitUnaryOperation(u)
}

type IfStatement struct {
	Condition Expression
	ThenBlock Block
	ElseBlock *Block // optional
}

func (i *IfStatement) Accept(visitor AstVisitor) {
	visitor.VisitIfStatement(i)
}

type WhileStatement struct {
	Condition Expression
	Body      Block
}

func (w *WhileStatement) Accept(visitor AstVisitor) {
	visitor.VisitWhileStatement(w)
}

type BreakStatement struct {
}

func (b *BreakStatement) Accept(visitor AstVisitor) {
	visitor.VisitBreakStatement(b)
}

type ContinueStatement struct {
}

func (c *ContinueStatement) Accept(visitor AstVisitor) {
	visitor.VisitContinueStatement(c)
}
