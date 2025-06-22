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
}

func (s *Statement) Accept(visitor AstVisitor) {
	if s.VariableDeclaration != nil {
		s.VariableDeclaration.Accept(visitor)
	} else if s.ExpressionStatement != nil {
		s.ExpressionStatement.Accept(visitor)
	} else if s.ReturnStatement != nil {
		s.ReturnStatement.Accept(visitor)
	}
}

// Helper functions for creating Statement unions
func NewVariableDeclarationStatement(variableDeclaration *VariableDeclaration) Statement {
	return Statement{VariableDeclaration: variableDeclaration}
}

func NewExpressionStatement(expressionStatement *ExpressionStatement) Statement {
	return Statement{ExpressionStatement: expressionStatement}
}

func NewReturnStatement(returnStatement *ReturnStatement) Statement {
	return Statement{ReturnStatement: returnStatement}
}

type Expression struct {
	Literal           *Literal
	Assignment        *Assignment
	FunctionCall      *FunctionCall
	VariableReference *VariableReference
	BinaryOperation   *BinaryOperation
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

func NewBinaryOperationExpression(binaryOperation *BinaryOperation) Expression {
	return Expression{BinaryOperation: binaryOperation}
}

type Literal struct {
	StringValue *string
	IntValue    *int
}

func (l *Literal) Accept(visitor AstVisitor) {
	visitor.VisitLiteral(l)
}

// Helper functions for creating Literal values
func NewIntLiteral(value int) *Literal {
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
