package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/functions"
	"github.com/iley/pirx/internal/parser"
)

const (
	INTLIT = "int literal"  // pseudo-type representing an untyped integer literal
)

var (
	supportedTypes = map[string]struct{}{
		"int": {},
		"bool": {},
		"string": {},
		"int32": {},
	}
)

type TypeChecker struct {
	declaredVars  map[string]string
	declaredFuncs map[string]functions.Proto
	errors        []error
	currentFunc   functions.Proto
	hasReturn     bool
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

func (c *TypeChecker) CheckFunction(fn parser.Function) {
	c.currentFunc = c.declaredFuncs[fn.Name]
	c.hasReturn = false
	c.declaredVars = make(map[string]string)

	for _, arg := range fn.Args {
		c.declaredVars[arg.Name] = arg.Type
	}

	c.CheckBlock(&fn.Body)

	// TODO: Check that each possible execution path ends with a return.
	if c.currentFunc.ReturnType != "" && !c.hasReturn {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s with return type %s must contain a return operator", fn.Loc.Line, fn.Loc.Col, c.currentFunc.Name, c.currentFunc.ReturnType))
	}
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
		return INTLIT
	} else if lit.BoolValue != nil {
		return "bool"
	}
	panic(fmt.Sprintf("unknown literal type: %v", *lit))
}

func (c *TypeChecker) CheckVariableDeclaration(decl *parser.VariableDeclaration) {
	if !isValidType(decl.Type) {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: unknown type %s", decl.Loc.Line, decl.Loc.Col, decl.Type))
	}
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
		if actualArgType != expectedArgType && !isConvertableTo(actualArgType, expectedArgType) {
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
	if valueType != varType && !isConvertableTo(valueType, varType) {
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
	c.hasReturn = true

	if stmt.Value == nil && c.currentFunc.ReturnType != "" {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s should return a value of type %s but no value was provided",
			stmt.Loc.Line, stmt.Loc.Col, c.currentFunc.Name, c.currentFunc.ReturnType,
		))
	}

	if stmt.Value != nil {
		typ := c.CheckExpression(stmt.Value)
		if c.currentFunc.ReturnType == "" {
			c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s does not have a return type but a value was provided",
				stmt.Loc.Line, stmt.Loc.Col, c.currentFunc.Name,
			))
		} else if typ != c.currentFunc.ReturnType && !isConvertableTo(typ, c.currentFunc.ReturnType) {
			c.errors = append(c.errors, fmt.Errorf("%d:%d: function %s has return type %s but a value of type %s was provided",
				stmt.Loc.Line, stmt.Loc.Col, c.currentFunc.Name, c.currentFunc.ReturnType, typ,
			))
		}
	}
}

func (c *TypeChecker) CheckBinaryOperation(binOp *parser.BinaryOperation) string {
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

func (c *TypeChecker) CheckUnaryOperation(unaryOp *parser.UnaryOperation) string {
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

func (c *TypeChecker) CheckIfStatement(stmt *parser.IfStatement) {
	exprType := c.CheckExpression(stmt.Condition)
	if exprType != "bool" {
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

func (c *TypeChecker) CheckWhileStatement(stmt *parser.WhileStatement) {
	exprType := c.CheckExpression(stmt.Condition)
	if exprType != "bool" {
		c.errors = append(c.errors, fmt.Errorf("%d:%d: expected an expression of type bool in while condition, got type %s",
			stmt.Loc.Line,
			stmt.Loc.Col,
			exprType,
		))
	}
	c.CheckBlock(&stmt.Body)
}

func (c *TypeChecker) CheckBreakStatement(stmt *parser.BreakStatement) {
	// noop
}

func (c *TypeChecker) CheckContinueStatement(stmt *parser.ContinueStatement) {
	// noop
}

func binaryOperationResult(op, left, right string) (string, bool) {
	if op == "==" || op == "!=" {
		// Equality is supported for all types.
		if left == right {
			return "bool", true
		} else if isIntegerType(left) && right == INTLIT {
			return "bool", true
		} else if isIntegerType(right) && left == INTLIT {
			return "bool", true
		}
		return "", false
	}

	if op == "+" || op == "-" || op == "/" || op == "*" || op == "%" {
		if isIntegerType(left) && isIntegerType(right) {
			// TODO: Add support for auto-promotion.
			// If both left and right are concrete types, they must match.
			return left, left == right
		} else if isIntegerType(left) && (isIntegerType(right) || right == INTLIT) {
			// If one of the two is a literal, use the type of the other.
			return left, true
		} else if isIntegerType(right) && left == INTLIT {
			// Same as above.
			return right, true
		} else if left == INTLIT && right == INTLIT {
			// Two literals make a literal.
			return INTLIT, true
		}
		return "", false
	}

	if op == "<" || op == ">" || op == "<=" || op == ">=" {
		// These are (currently) supproted for integers only.
		return "bool", (isIntegerType(left) || left == INTLIT) && (isIntegerType(right) || right == INTLIT)
	}

	if op == "&&" || op == "||" {
		return "bool", left == "bool"
	}

	panic(fmt.Sprintf("unknown binary operation %s", op))
}

func unaryOperationResult(op, val string) (string, bool) {
	if op == "!" {
		return "bool", val == "bool"
	}
	panic(fmt.Sprintf("unknown binary operation %s", op))
}

func isValidType(name string) bool {
	_, ok := supportedTypes[name]
	return ok
}

func isIntegerType(name string) bool {
	// TODO: Add more numeric types.
	return name == "int" || name == "int32"
}

func isConvertableTo(from, to string) bool {
	return isIntegerType(to) && from == INTLIT
}
