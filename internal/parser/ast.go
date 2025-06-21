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
}

func (s *Statement) Accept(visitor AstVisitor) {
	if s.VariableDeclaration != nil {
		s.VariableDeclaration.Accept(visitor)
	} else if s.ExpressionStatement != nil {
		s.ExpressionStatement.Accept(visitor)
	}
}

// Helper functions for creating Statement unions
func NewVariableDeclarationStatement(variableDeclaration *VariableDeclaration) Statement {
	return Statement{VariableDeclaration: variableDeclaration}
}

func NewExpressionStatement(expressionStatement *ExpressionStatement) Statement {
	return Statement{ExpressionStatement: expressionStatement}
}

type Expression struct {
	Literal           *Literal
	Assignment        *Assignment
	FunctionCall      *FunctionCall
	VariableReference *VariableReference
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
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", e))
	}
}

// Helper functions for creating Expression unions
func NewLiteralExpression(literal *Literal) Expression {
	return Expression{Literal: literal}
}

func NewFunctionCallExpression(functionCall *FunctionCall) Expression {
	return Expression{FunctionCall: functionCall}
}

func NewAssignmentExpression(assignment *Assignment) Expression {
	return Expression{Assignment: assignment}
}

func NewVariableReferenceExpression(variableReference *VariableReference) Expression {
	return Expression{VariableReference: variableReference}
}

type LiteralType int

const (
	LiteralTypeString LiteralType = iota
	LiteralTypeInt
)

type Literal struct {
	Type        LiteralType
	StringValue string
	IntValue    int
}

func (l *Literal) Accept(visitor AstVisitor) {
	visitor.VisitLiteral(l)
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
