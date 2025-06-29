package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/parser"
)

type VariableChecker struct {
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

func (c *VariableChecker) CheckProgram(program *parser.Program) {
	for _, fn := range program.Functions {
		c.CheckFunction(fn)
	}
}

func (c *VariableChecker) CheckFunction(fn *parser.Function) {
	c.declaredVars = make(map[string]string)
	for _, param := range fn.Params {
		c.declaredVars[param.Name] = param.Type
	}

	c.CheckBlock(&fn.Body)
}

func (c *VariableChecker) CheckBlock(block *parser.Block) {
	for _, stmt := range block.Statements {
		c.CheckStatement(&stmt)
	}
}

func (c *VariableChecker) CheckStatement(stmt *parser.Statement) {
	if stmt.VariableDeclaration != nil {
		c.CheckVariableDeclaration(stmt.VariableDeclaration)
	} else if stmt.ExpressionStatement != nil {
		c.CheckExpressionStatement(stmt.ExpressionStatement)
	} else if stmt.ReturnStatement != nil {
		c.CheckReturnStatement(stmt.ReturnStatement)
	} else if stmt.IfStatement != nil {
		c.CheckIfStatement(stmt.IfStatement)
	} else if stmt.WhileStatement != nil {
		c.CheckWhileStatement(stmt.WhileStatement)
	} else if stmt.BreakStatement != nil {
		c.CheckBreakStatement(stmt.BreakStatement)
	} else if stmt.ContinueStatement != nil {
		c.CheckContinueStatement(stmt.ContinueStatement)
	} else {
		panic(fmt.Sprintf("unsupported statement type: %v", *stmt))
	}
}

func (c *VariableChecker) CheckExpression(expr *parser.Expression) {
	if expr.Literal != nil {
		c.CheckLiteral(expr.Literal)
	} else if expr.Assignment != nil {
		c.CheckAssignment(expr.Assignment)
	} else if expr.FunctionCall != nil {
		c.CheckFunctionCall(expr.FunctionCall)
	} else if expr.VariableReference != nil {
		c.CheckVariableReference(expr.VariableReference)
	} else if expr.BinaryOperation != nil {
		c.CheckBinaryOperation(expr.BinaryOperation)
	} else if expr.UnaryOperation != nil {
		c.CheckUnaryOperation(expr.UnaryOperation)
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", expr))
	}
}

func (c *VariableChecker) CheckLiteral(literal *parser.Literal) {
	// noop
}

func (c *VariableChecker) CheckVariableDeclaration(decl *parser.VariableDeclaration) {
	c.declaredVars[decl.Name] = decl.Type
}

func (c *VariableChecker) CheckFunctionCall(functionCall *parser.FunctionCall) {
	for _, expr := range functionCall.Args {
		c.CheckExpression(&expr)
	}
}

func (c *VariableChecker) CheckExpressionStatement(e *parser.ExpressionStatement) {
	c.CheckExpression(&e.Expression)
}

func (c *VariableChecker) CheckAssignment(assignment *parser.Assignment) {
	target := assignment.VariableName
	_, declared := c.declaredVars[target]
	if !declared {
		// TODO: Line and column.
		c.errors = append(c.errors, fmt.Errorf("variable %s is not declared before assignment", target))
	}

	c.CheckExpression(&assignment.Value)
}

func (c *VariableChecker) CheckVariableReference(ref *parser.VariableReference) {
	_, declared := c.declaredVars[ref.Name]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("variable %s is not declared before reference", ref.Name))
	}
}

func (c *VariableChecker) CheckReturnStatement(stmt *parser.ReturnStatement) {
	if stmt.Value != nil {
		c.CheckExpression(stmt.Value)
	}
}

func (c *VariableChecker) CheckBinaryOperation(binOp *parser.BinaryOperation) {
	c.CheckExpression(&binOp.Left)
	c.CheckExpression(&binOp.Right)
}

func (c *VariableChecker) CheckUnaryOperation(unaryOp *parser.UnaryOperation) {
	c.CheckExpression(&unaryOp.Operand)
}

func (c *VariableChecker) CheckIfStatement(stmt *parser.IfStatement) {
	c.CheckExpression(&stmt.Condition)
	c.CheckBlock(&stmt.ThenBlock)
	if stmt.ElseBlock != nil {
		c.CheckBlock(stmt.ElseBlock)
	}
}

func (c *VariableChecker) CheckWhileStatement(stmt *parser.WhileStatement) {
	c.CheckExpression(&stmt.Condition)
	c.CheckBlock(&stmt.Body)
}

func (c *VariableChecker) CheckBreakStatement(stmt *parser.BreakStatement) {
	// noop
}

func (c *VariableChecker) CheckContinueStatement(stmt *parser.ContinueStatement) {
	// noop
}
