package parser

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

type Statement interface {
	Accept(visitor AstVisitor)
}

type Expression interface {
	Accept(visitor AstVisitor)
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
