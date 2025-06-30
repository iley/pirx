package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/functions"
	"github.com/iley/pirx/internal/parser"
)

type TypeChecker struct {
	declaredVars  map[string]string
	declaredFuncs map[string]functions.Proto
	errors        []error
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		declaredVars:  make(map[string]string),
		declaredFuncs: make(map[string]functions.Proto),
		errors:        []error{},
	}
}

func (c *TypeChecker) Errors() []error {
	return c.errors
}

func (c *TypeChecker) CheckProgram(program *parser.Program) {
	// Gather function prototypes so we can check arguments and types later.
	protos := functions.GetFunctionTable(program)
	for _, proto := range protos {
		c.declaredFuncs[proto.Name] = proto
	}

	for _, fn := range program.Functions {
		c.CheckFunction(fn)
	}
}

func (c *TypeChecker) CheckFunction(fn parser.Function) {
	c.declaredVars = make(map[string]string)
	for _, arg := range fn.Args {
		c.declaredVars[arg.Name] = arg.Type
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

func (c *TypeChecker) CheckExpression(expr parser.Expression) string {
	if literal, ok := expr.(*parser.Literal); ok {
		return c.CheckLiteral(literal)
	} else if assignment, ok := expr.(*parser.Assignment); ok {
		return c.CheckAssignment(assignment)
	} else if functionCall, ok := expr.(*parser.FunctionCall); ok {
		return c.CheckFunctionCall(functionCall)
	} else if variableReference, ok := expr.(*parser.VariableReference); ok {
		return c.CheckVariableReference(variableReference)
	} else if binaryOperation, ok := expr.(*parser.BinaryOperation); ok {
		return c.CheckBinaryOperation(binaryOperation)
	} else if unaryOperation, ok := expr.(*parser.UnaryOperation); ok {
		return c.CheckUnaryOperation(unaryOperation)
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", expr))
	}
}

func (c *TypeChecker) CheckLiteral(lit *parser.Literal) string {
	if lit.StringValue != nil {
		return "string"
	} else if lit.IntValue != nil {
		return "int"
	}
	panic(fmt.Sprintf("unknown literal type: %v", *lit))
}

func (c *TypeChecker) CheckVariableDeclaration(decl *parser.VariableDeclaration) {
	c.declaredVars[decl.Name] = decl.Type
}

func (c *TypeChecker) CheckFunctionCall(call *parser.FunctionCall) string {
	proto, declared := c.declaredFuncs[call.FunctionName]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s is not declared", call.Loc.Line, call.Loc.Col, call.FunctionName))
	}

	if declared && !proto.Variadic && (len(proto.Args) != len(call.Args)) {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s has %d arguments but %d were provided", call.Loc.Line, call.Loc.Col, call.FunctionName, len(proto.Args), len(call.Args)))
	}

	for i, expr := range call.Args {
		actualArgType := c.CheckExpression(expr)

		if i >= len(proto.Args) {
			continue
		}

		expectedArgType := proto.Args[i].Typ
		if actualArgType != expectedArgType {
			c.errors = append(c.errors, fmt.Errorf("%d:%d: argument #%d of function %s has wrong type: expected %s but got %s",
				call.Loc.Line, call.Loc.Col, i+1, call.FunctionName, expectedArgType, actualArgType))
		}
	}

	return proto.ReturnType
}

func (c *TypeChecker) CheckExpressionStatement(e *parser.ExpressionStatement) {
	c.CheckExpression(e.Expression)
}

func (c *TypeChecker) CheckAssignment(assignment *parser.Assignment) string {
	target := assignment.VariableName
	varType, declared := c.declaredVars[target]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: variable %s is not declared before assignment", assignment.Loc.Line, assignment.Loc.Col, target))
	}

	valueType := c.CheckExpression(assignment.Value)
	if valueType != varType {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: cannot assign value of type %s to variable %s of type %s",
			assignment.Loc.Line,
			assignment.Loc.Col,
			valueType,
			target,
			varType,
		))
	}

	return varType
}

func (c *TypeChecker) CheckVariableReference(ref *parser.VariableReference) string {
	varType, declared := c.declaredVars[ref.Name]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: variable %s is not declared before reference", ref.Loc.Line, ref.Loc.Col, ref.Name))
	}

	return varType
}

func (c *TypeChecker) CheckReturnStatement(stmt *parser.ReturnStatement) {
	if stmt.Value != nil {
		c.CheckExpression(stmt.Value)
	}
}

func (c *TypeChecker) CheckBinaryOperation(binOp *parser.BinaryOperation) string {
	leftType := c.CheckExpression(binOp.Left)
	rightType := c.CheckExpression(binOp.Right)
	// TODO: Check that the types are appropriate for the operation.
	if leftType != rightType {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: cannot apply operation %s to operands of different types (%s and %s)",
			binOp.Loc.Line,
			binOp.Loc.Col,
			binOp.Operator,
			leftType,
			rightType,
		))
	}
	return leftType
}

func (c *TypeChecker) CheckUnaryOperation(unaryOp *parser.UnaryOperation) string {
	// TODO: Check that the operation is appropriate for the type.
	return c.CheckExpression(unaryOp.Operand)
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
