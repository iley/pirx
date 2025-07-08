package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/functions"
	"github.com/iley/pirx/internal/types"
)

type TypeChecker struct {
	declaredVars  map[string]types.Type
	declaredFuncs map[string]functions.Proto
	errors        []error
	currentFunc   functions.Proto
	hasReturn     bool
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		declaredVars:  make(map[string]types.Type),
		declaredFuncs: make(map[string]functions.Proto),
		errors:        []error{},
	}
}

func (c *TypeChecker) Errors() []error {
	return c.errors
}

func (c *TypeChecker) CheckProgram(program *ast.Program) {
	// TODO: Check that function declarations use valid types.

	// Gather function prototypes so we can check arguments and types later.
	protos := functions.GetFunctionTable(program)
	for _, proto := range protos {
		c.declaredFuncs[proto.Name] = proto
	}

	for _, fn := range program.Functions {
		c.CheckFunction(fn)
	}
}

func (c *TypeChecker) CheckFunction(fn ast.Function) {
	c.currentFunc = c.declaredFuncs[fn.Name]
	c.hasReturn = false
	c.declaredVars = make(map[string]types.Type)

	for _, arg := range fn.Args {
		c.declaredVars[arg.Name] = arg.Type
	}

	c.CheckBlock(&fn.Body)

	// TODO: Check that each possible execution path ends with a return.
	if c.currentFunc.ReturnType != nil && !c.hasReturn {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s with return type %s must contain a return operator", fn.Loc.Line, fn.Loc.Col, c.currentFunc.Name, c.currentFunc.ReturnType))
	}
}

func (c *TypeChecker) CheckBlock(block *ast.Block) {
	for _, stmt := range block.Statements {
		c.CheckStatement(stmt)
	}
}

func (c *TypeChecker) CheckStatement(stmt ast.Statement) {
	if varDecl, ok := stmt.(*ast.VariableDeclaration); ok {
		c.CheckVariableDeclaration(varDecl)
	} else if exprStmt, ok := stmt.(*ast.ExpressionStatement); ok {
		c.CheckExpressionStatement(exprStmt)
	} else if retStmt, ok := stmt.(*ast.ReturnStatement); ok {
		c.CheckReturnStatement(retStmt)
	} else if ifStmt, ok := stmt.(*ast.IfStatement); ok {
		c.CheckIfStatement(ifStmt)
	} else if whileStmt, ok := stmt.(*ast.WhileStatement); ok {
		c.CheckWhileStatement(whileStmt)
	} else if breakStmt, ok := stmt.(*ast.BreakStatement); ok {
		c.CheckBreakStatement(breakStmt)
	} else if contStmt, ok := stmt.(*ast.ContinueStatement); ok {
		c.CheckContinueStatement(contStmt)
	} else {
		panic(fmt.Sprintf("unsupported statement type: %v", stmt))
	}
}

func (c *TypeChecker) CheckExpression(expr ast.Expression) types.Type {
	if literal, ok := expr.(*ast.Literal); ok {
		return c.CheckLiteral(literal)
	} else if assignment, ok := expr.(*ast.Assignment); ok {
		return c.CheckAssignment(assignment)
	} else if functionCall, ok := expr.(*ast.FunctionCall); ok {
		return c.CheckFunctionCall(functionCall)
	} else if variableReference, ok := expr.(*ast.VariableReference); ok {
		return c.CheckVariableReference(variableReference)
	} else if binaryOperation, ok := expr.(*ast.BinaryOperation); ok {
		return c.CheckBinaryOperation(binaryOperation)
	} else if unaryOperation, ok := expr.(*ast.UnaryOperation); ok {
		return c.CheckUnaryOperation(unaryOperation)
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", expr))
	}
}

func (c *TypeChecker) CheckLiteral(lit *ast.Literal) types.Type {
	if lit.StringValue != nil {
		return types.String
	} else if lit.IntValue != nil {
		return types.Int
	} else if lit.Int64Value != nil {
		return types.Int64
	} else if lit.BoolValue != nil {
		return types.Bool
	}
	panic(fmt.Sprintf("unknown literal type: %v", *lit))
}

func (c *TypeChecker) CheckVariableDeclaration(decl *ast.VariableDeclaration) {
	c.declaredVars[decl.Name] = decl.Type
}

func (c *TypeChecker) CheckFunctionCall(call *ast.FunctionCall) types.Type {
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
		if !actualArgType.Equals(expectedArgType) {
			c.errors = append(c.errors, fmt.Errorf("%d:%d: argument #%d of function %s has wrong type: expected %s but got %s",
				call.Loc.Line, call.Loc.Col, i+1, call.FunctionName, expectedArgType, actualArgType))
		}
	}

	return proto.ReturnType
}

func (c *TypeChecker) CheckExpressionStatement(e *ast.ExpressionStatement) {
	c.CheckExpression(e.Expression)
}

func (c *TypeChecker) CheckAssignment(assignment *ast.Assignment) types.Type {
	target := assignment.VariableName
	varType, declared := c.declaredVars[target]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: variable %s is not declared before assignment", assignment.Loc.Line, assignment.Loc.Col, target))
	}

	valueType := c.CheckExpression(assignment.Value)
	if !valueType.Equals(varType) {
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

func (c *TypeChecker) CheckVariableReference(ref *ast.VariableReference) types.Type {
	varType, declared := c.declaredVars[ref.Name]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: variable %s is not declared before reference", ref.Loc.Line, ref.Loc.Col, ref.Name))
	}

	return varType
}

func (c *TypeChecker) CheckReturnStatement(stmt *ast.ReturnStatement) {
	c.hasReturn = true

	if stmt.Value == nil && c.currentFunc.ReturnType != nil {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s should return a value of type %s but no value was provided",
			stmt.Loc.Line, stmt.Loc.Col, c.currentFunc.Name, c.currentFunc.ReturnType,
		))
	}

	if stmt.Value != nil {
		typ := c.CheckExpression(stmt.Value)
		if c.currentFunc.ReturnType == nil {
			c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s does not have a return type but a value was provided",
				stmt.Loc.Line, stmt.Loc.Col, c.currentFunc.Name,
			))
		} else if !typ.Equals(c.currentFunc.ReturnType) {
			c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s has return type %s but a value of type %s was provided",
				stmt.Loc.Line, stmt.Loc.Col, c.currentFunc.Name, c.currentFunc.ReturnType, typ,
			))
		}
	}
}

func (c *TypeChecker) CheckBinaryOperation(binOp *ast.BinaryOperation) types.Type {
	leftType := c.CheckExpression(binOp.Left)
	rightType := c.CheckExpression(binOp.Right)
	resultType, ok := binaryOperationResult(binOp.Operator, leftType, rightType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: binary operation %s cannot be applied to values of types %s and %s",
			binOp.Loc.Line,
			binOp.Loc.Col,
			binOp.Operator,
			leftType,
			rightType,
		))
	}
	return resultType
}

func (c *TypeChecker) CheckUnaryOperation(unaryOp *ast.UnaryOperation) types.Type {
	operandType := c.CheckExpression(unaryOp.Operand)
	resultType, ok := unaryOperationResult(unaryOp.Operator, operandType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: unary operation %s cannot be applied to a value of type %s",
			unaryOp.Loc.Line,
			unaryOp.Loc.Col,
			unaryOp.Operator,
			operandType,
		))
	}
	return resultType
}

func (c *TypeChecker) CheckIfStatement(stmt *ast.IfStatement) {
	exprType := c.CheckExpression(stmt.Condition)
	if exprType != types.Bool {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: expected an expression of type bool in if condition, got type %s",
			stmt.Loc.Line,
			stmt.Loc.Col,
			exprType,
		))
	}
	c.CheckBlock(&stmt.ThenBlock)
	if stmt.ElseBlock != nil {
		c.CheckBlock(stmt.ElseBlock)
	}
}

func (c *TypeChecker) CheckWhileStatement(stmt *ast.WhileStatement) {
	exprType := c.CheckExpression(stmt.Condition)
	if exprType != types.Bool {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: expected an expression of type bool in while condition, got type %s",
			stmt.Loc.Line,
			stmt.Loc.Col,
			exprType,
		))
	}
	c.CheckBlock(&stmt.Body)
}

func (c *TypeChecker) CheckBreakStatement(stmt *ast.BreakStatement) {
	// noop
}

func (c *TypeChecker) CheckContinueStatement(stmt *ast.ContinueStatement) {
	// noop
}

func binaryOperationResult(op string, left, right types.Type) (types.Type, bool) {
	if !left.Equals(right) {
		return nil, false
	}

	if op == "==" || op == "!=" {
		// Equality is supported for all types.
		return types.Bool, true
	}

	if op == "+" || op == "-" || op == "/" || op == "*" || op == "%" {
		// These are (currently) supproted for integers only.
		return left, left == types.Int || left == types.Int64
	}

	if op == "<" || op == ">" || op == "<=" || op == ">=" {
		// These are (currently) supproted for integers only.
		return types.Bool, left == types.Int || left == types.Int64
	}

	if op == "&&" || op == "||" {
		return types.Bool, left == types.Bool
	}

	panic(fmt.Sprintf("unknown binary operation %s", op))
}

func unaryOperationResult(op string, val types.Type) (types.Type, bool) {
	switch op {
	case "!":
		return types.Bool, val == types.Bool
	case "-":
		return val, val == types.Int || val == types.Int64
	}
	panic(fmt.Sprintf("unknown binary operation %s", op))
}
