package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/parser"
)

type TypeChecker struct {
	declaredVars map[string]string
	errors       []error
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		declaredVars: make(map[string]string),
		errors:       []error{},
	}
}

func (c *TypeChecker) Success() bool {
	return len(c.errors) == 0
}

func (c *TypeChecker) Errors() []error {
	return c.errors
}

func (c *TypeChecker) CheckProgram(program *parser.Program) {
	for _, fn := range program.Functions {
		c.CheckFunction(fn)
	}
}

func (c *TypeChecker) CheckFunction(fn parser.Function) {
	c.declaredVars = make(map[string]string)
	for _, param := range fn.Params {
		c.declaredVars[param.Name] = param.Type
	}

	c.CheckBlock(&fn.Body)
}

func (c *TypeChecker) CheckBlock(block *parser.Block) {
	for _, stmt := range block.Statements {
		c.CheckStatement(stmt)
	}
}

func (c *TypeChecker) CheckStatement(stmt parser.Statement) {
	if varDecl, ok := stmt.(*parser.VariableDeclaration); ok {
		c.CheckVariableDeclaration(varDecl)
	} else if exprStmt, ok := stmt.(*parser.ExpressionStatement); ok {
		c.CheckExpressionStatement(exprStmt)
	} else if retStmt, ok := stmt.(*parser.ReturnStatement); ok {
		c.CheckReturnStatement(retStmt)
	} else if ifStmt, ok := stmt.(*parser.IfStatement); ok {
		c.CheckIfStatement(ifStmt)
	} else if whileStmt, ok := stmt.(*parser.WhileStatement); ok {
		c.CheckWhileStatement(whileStmt)
	} else if breakStmt, ok := stmt.(*parser.BreakStatement); ok {
		c.CheckBreakStatement(breakStmt)
	} else if contStmt, ok := stmt.(*parser.ContinueStatement); ok {
		c.CheckContinueStatement(contStmt)
	} else {
		panic(fmt.Sprintf("unsupported statement type: %v", stmt))
	}
}

func (c *TypeChecker) CheckExpression(expr parser.Expression) {
	if literal, ok := expr.(*parser.Literal); ok {
		c.CheckLiteral(literal)
	} else if assignment, ok := expr.(*parser.Assignment); ok {
		c.CheckAssignment(assignment)
	} else if functionCall, ok := expr.(*parser.FunctionCall); ok {
		c.CheckFunctionCall(functionCall)
	} else if variableReference, ok := expr.(*parser.VariableReference); ok {
		c.CheckVariableReference(variableReference)
	} else if binaryOperation, ok := expr.(*parser.BinaryOperation); ok {
		c.CheckBinaryOperation(binaryOperation)
	} else if unaryOperation, ok := expr.(*parser.UnaryOperation); ok {
		c.CheckUnaryOperation(unaryOperation)
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", expr))
	}
}

func (c *TypeChecker) CheckLiteral(literal *parser.Literal) {
	// noop
}

func (c *TypeChecker) CheckVariableDeclaration(decl *parser.VariableDeclaration) {
	c.declaredVars[decl.Name] = decl.Type
}

func (c *TypeChecker) CheckFunctionCall(functionCall *parser.FunctionCall) {
	for _, expr := range functionCall.Args {
		c.CheckExpression(expr)
	}
}

func (c *TypeChecker) CheckExpressionStatement(e *parser.ExpressionStatement) {
	c.CheckExpression(e.Expression)
}

func (c *TypeChecker) CheckAssignment(assignment *parser.Assignment) {
	target := assignment.VariableName
	_, declared := c.declaredVars[target]
	if !declared {
		// TODO: Line and column.
		c.errors = append(c.errors, fmt.Errorf("variable %s is not declared before assignment", target))
	}

	c.CheckExpression(assignment.Value)
}

func (c *TypeChecker) CheckVariableReference(ref *parser.VariableReference) {
	_, declared := c.declaredVars[ref.Name]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("variable %s is not declared before reference", ref.Name))
	}
}

func (c *TypeChecker) CheckReturnStatement(stmt *parser.ReturnStatement) {
	if stmt.Value != nil {
		c.CheckExpression(stmt.Value)
	}
}

func (c *TypeChecker) CheckBinaryOperation(binOp *parser.BinaryOperation) {
	c.CheckExpression(binOp.Left)
	c.CheckExpression(binOp.Right)
}

func (c *TypeChecker) CheckUnaryOperation(unaryOp *parser.UnaryOperation) {
	c.CheckExpression(unaryOp.Operand)
}

func (c *TypeChecker) CheckIfStatement(stmt *parser.IfStatement) {
	c.CheckExpression(stmt.Condition)
	c.CheckBlock(&stmt.ThenBlock)
	if stmt.ElseBlock != nil {
		c.CheckBlock(stmt.ElseBlock)
	}
}

func (c *TypeChecker) CheckWhileStatement(stmt *parser.WhileStatement) {
	c.CheckExpression(stmt.Condition)
	c.CheckBlock(&stmt.Body)
}

func (c *TypeChecker) CheckBreakStatement(stmt *parser.BreakStatement) {
	// noop
}

func (c *TypeChecker) CheckContinueStatement(stmt *parser.ContinueStatement) {
	// noop
}
