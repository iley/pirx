package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/parser"
)

type VariableChecker struct {
	parser.AstVisitor
	declaredVars map[string]string
	errors       []error
}

func NewVariableChecker() *VariableChecker {
	return &VariableChecker{
		declaredVars: make(map[string]string),
		errors:       []error{},
	}
}

func (c *VariableChecker) Success() bool {
	return len(c.errors) == 0
}

func (c *VariableChecker) Errors() []error {
	return c.errors
}

func (c *VariableChecker) VisitProgram(program *parser.Program) {
	for _, fn := range program.Functions {
		fn.Accept(c)
	}
}

func (c *VariableChecker) VisitFunction(fn *parser.Function) {
	c.declaredVars = make(map[string]string)
	for _, param := range fn.Params {
		c.declaredVars[param.Name] = param.Type
	}

	if fn.Body == nil {
		return
	}

	for _, stmt := range fn.Body.Statements {
		stmt.Accept(c)
	}
}

func (c *VariableChecker) VisitStatement(stmt parser.Statement) {
	stmt.Accept(c)
}

func (c *VariableChecker) VisitLiteral(literal *parser.Literal) {
	// noop
}

func (c *VariableChecker) VisitVariableDeclaration(decl *parser.VariableDeclaration) {
	c.declaredVars[decl.Name] = decl.Type
}

func (c *VariableChecker) VisitFunctionCall(functionCall *parser.FunctionCall) {
	for _, expr := range functionCall.Args {
		expr.Accept(c)
	}
}

func (c *VariableChecker) VisitExpressionStatement(e *parser.ExpressionStatement) {
	e.Expression.Accept(c)
}

func (c *VariableChecker) VisitAssignment(assignment *parser.Assignment) {
	target := assignment.VariableName
	_, declared := c.declaredVars[target]
	if !declared {
		// TODO: Line and column.
		c.errors = append(c.errors, fmt.Errorf("variable %s is not declared before assignment", target))
	}

	assignment.Value.Accept(c)
}

func (c *VariableChecker) VisitVariableReference(ref *parser.VariableReference) {
	_, declared := c.declaredVars[ref.Name]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("variable %s is not declared before reference", ref.Name))
	}
}

func (c *VariableChecker) VisitReturnStatement(stmt *parser.ReturnStatement) {
	stmt.Value.Accept(c)
}

func (c *VariableChecker) VisitBinaryOperation(binOp *parser.BinaryOperation) {
	binOp.Left.Accept(c)
	binOp.Right.Accept(c)
}

func (c *VariableChecker) VisitUnaryOperation(unaryOp *parser.UnaryOperation) {
	unaryOp.Operand.Accept(c)
}

func (c *VariableChecker) VisitIfStatement(stmt *parser.IfStatement) {
	stmt.Condition.Accept(c)
}

func (c *VariableChecker) VisitWhileStatement(stmt *parser.WhileStatement) {
	stmt.Condition.Accept(c)
}

func (c *VariableChecker) VisitBreakStatement(stmt *parser.BreakStatement) {
	// noop
}

func (c *VariableChecker) VisitContinueStatement(stmt *parser.ContinueStatement) {
	// noop
}
