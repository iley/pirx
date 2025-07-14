package checks

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/types"
)

type TypeChecker struct {
	program       *ast.Program
	declaredVars  map[string]ast.Type
	declaredFuncs map[string]types.FuncProto
	types         *types.TypeTable
	errors        []error
	currentFunc   types.FuncProto
	hasReturn     bool
}

func NewTypeChecker(program *ast.Program) *TypeChecker {
	return &TypeChecker{
		program:       program,
		declaredVars:  make(map[string]ast.Type),
		declaredFuncs: make(map[string]types.FuncProto),
		errors:        []error{},
	}
}

func (c *TypeChecker) Errors() []error {
	return c.errors
}

func (c *TypeChecker) Check() {
	// TODO: Check that function declarations use valid types.

	// Gather function prototypes so we can check arguments and types later.
	protos := types.GetFunctionTable(c.program)
	for _, proto := range protos {
		c.declaredFuncs[proto.Name] = proto
	}

	types, err := types.MakeTypeTable(c.program.TypeDeclarations)
	if err != nil {
		c.errors = append(c.errors, err)
	}
	c.types = types

	for _, fn := range c.program.Functions {
		c.checkFunction(fn)
	}
}

func (c *TypeChecker) checkFunction(fn ast.Function) {
	c.currentFunc = c.declaredFuncs[fn.Name]
	c.hasReturn = false
	c.declaredVars = make(map[string]ast.Type)

	for _, arg := range fn.Args {
		c.declaredVars[arg.Name] = arg.Type
	}

	c.checkBlock(&fn.Body)

	// TODO: Check that each possible execution path ends with a return.
	if c.currentFunc.ReturnType != nil && !c.hasReturn {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s with return type %s must contain a return operator", fn.Loc, c.currentFunc.Name, c.currentFunc.ReturnType))
	}
}

func (c *TypeChecker) checkBlock(block *ast.Block) {
	for _, stmt := range block.Statements {
		c.checkStatement(stmt)
	}
}

func (c *TypeChecker) checkStatement(stmt ast.Statement) {
	if varDecl, ok := stmt.(*ast.VariableDeclaration); ok {
		c.checkVariableDeclaration(varDecl)
	} else if exprStmt, ok := stmt.(*ast.ExpressionStatement); ok {
		c.checkExpressionStatement(exprStmt)
	} else if retStmt, ok := stmt.(*ast.ReturnStatement); ok {
		c.checkReturnStatement(retStmt)
	} else if ifStmt, ok := stmt.(*ast.IfStatement); ok {
		c.checkIfStatement(ifStmt)
	} else if whileStmt, ok := stmt.(*ast.WhileStatement); ok {
		c.checkWhileStatement(whileStmt)
	} else if breakStmt, ok := stmt.(*ast.BreakStatement); ok {
		c.checkBreakStatement(breakStmt)
	} else if contStmt, ok := stmt.(*ast.ContinueStatement); ok {
		c.checkContinueStatement(contStmt)
	} else {
		panic(fmt.Sprintf("unsupported statement type: %v", stmt))
	}
}

func (c *TypeChecker) checkExpression(expr ast.Expression) ast.Type {
	if literal, ok := expr.(*ast.Literal); ok {
		return c.checkLiteral(literal)
	} else if assignment, ok := expr.(*ast.Assignment); ok {
		return c.checkAssignment(assignment)
	} else if functionCall, ok := expr.(*ast.FunctionCall); ok {
		return c.checkFunctionCall(functionCall)
	} else if variableReference, ok := expr.(*ast.VariableReference); ok {
		return c.checkVariableReference(variableReference)
	} else if binaryOperation, ok := expr.(*ast.BinaryOperation); ok {
		return c.checkBinaryOperation(binaryOperation)
	} else if unaryOperation, ok := expr.(*ast.UnaryOperation); ok {
		return c.checkUnaryOperation(unaryOperation)
	} else if lvalue, ok := expr.(*ast.FieldLValue); ok {
		return c.checkFieldAccess(lvalue)
	} else {
		panic(fmt.Sprintf("Invalid expression type: %v", expr))
	}
}

func (c *TypeChecker) checkLiteral(lit *ast.Literal) ast.Type {
	if lit.StringValue != nil {
		return ast.String
	} else if lit.IntValue != nil {
		return ast.Int
	} else if lit.Int64Value != nil {
		return ast.Int64
	} else if lit.BoolValue != nil {
		return ast.Bool
	}
	panic(fmt.Sprintf("unknown literal type: %v", *lit))
}

func (c *TypeChecker) checkVariableDeclaration(decl *ast.VariableDeclaration) {
	c.declaredVars[decl.Name] = decl.Type
}

func (c *TypeChecker) checkFunctionCall(call *ast.FunctionCall) ast.Type {
	proto, declared := c.declaredFuncs[call.FunctionName]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s is not declared", call.Loc, call.FunctionName))
	}

	if declared && !proto.Variadic && (len(proto.Args) != len(call.Args)) {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s has %d arguments but %d were provided", call.Loc, call.FunctionName, len(proto.Args), len(call.Args)))
	}

	for i, expr := range call.Args {
		actualArgType := c.checkExpression(expr)

		if i >= len(proto.Args) {
			continue
		}

		expectedArgType := proto.Args[i].Typ
		if !actualArgType.Equals(expectedArgType) {
			c.errors = append(c.errors, fmt.Errorf("%s: argument #%d of function %s has wrong type: expected %s but got %s",
				call.Loc, i+1, call.FunctionName, expectedArgType, actualArgType))
		}
	}

	return proto.ReturnType
}

func (c *TypeChecker) checkExpressionStatement(e *ast.ExpressionStatement) {
	c.checkExpression(e.Expression)
}

func (c *TypeChecker) checkAssignment(assignment *ast.Assignment) ast.Type {
	var targetType ast.Type
	if targetVar, ok := assignment.Target.(*ast.VariableLValue); ok {
		varName := targetVar.Name
		var declared bool
		targetType, declared = c.declaredVars[varName]
		if !declared {
			c.errors = append(c.errors, fmt.Errorf("%s: variable %s is not declared before assignment", assignment.Loc, varName))
			return nil
		}
	} else if deref, ok := assignment.Target.(*ast.DereferenceLValue); ok {
		refType := c.checkExpression(deref.Expression)
		ptrType, ok := refType.(*ast.PointerType)
		if !ok {
			c.errors = append(c.errors, fmt.Errorf("%s: dereference of a non-pointer type %s", assignment.Loc, refType))
			return nil
		}
		targetType = ptrType.ElementType
	} else if lvalue, ok := assignment.Target.(*ast.FieldLValue); ok {
		return c.checkFieldAccess(lvalue)
	} else {
		panic(fmt.Errorf("%s: invalid lvalue %s in assignment", assignment.Loc, assignment.Target))
	}

	valueType := c.checkExpression(assignment.Value)
	if !valueType.Equals(targetType) {
		c.errors = append(c.errors, fmt.Errorf("%s: cannot assign value of type %s to lvalue of type %s",
			assignment.Loc,
			valueType,
			targetType,
		))
	}

	return targetType
}

func (c *TypeChecker) checkVariableReference(ref *ast.VariableReference) ast.Type {
	varType, declared := c.declaredVars[ref.Name]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%s: variable %s is not declared before reference", ref.Loc, ref.Name))
	}

	return varType
}

func (c *TypeChecker) checkReturnStatement(stmt *ast.ReturnStatement) {
	c.hasReturn = true

	if stmt.Value == nil && c.currentFunc.ReturnType != nil {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s should return a value of type %s but no value was provided",
			stmt.Loc, c.currentFunc.Name, c.currentFunc.ReturnType,
		))
	}

	if stmt.Value != nil {
		typ := c.checkExpression(stmt.Value)
		if c.currentFunc.ReturnType == nil {
			c.errors = append(c.errors, fmt.Errorf("%s: function %s does not have a return type but a value was provided",
				stmt.Loc, c.currentFunc.Name,
			))
		} else if !typ.Equals(c.currentFunc.ReturnType) {
			c.errors = append(c.errors, fmt.Errorf("%s: function %s has return type %s but a value of type %s was provided",
				stmt.Loc, c.currentFunc.Name, c.currentFunc.ReturnType, typ,
			))
		}
	}
}

func (c *TypeChecker) checkBinaryOperation(binOp *ast.BinaryOperation) ast.Type {
	leftType := c.checkExpression(binOp.Left)
	rightType := c.checkExpression(binOp.Right)
	resultType, ok := binaryOperationResult(binOp.Operator, leftType, rightType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: binary operation %s cannot be applied to values of types %s and %s",
			binOp.Loc,
			binOp.Operator,
			leftType,
			rightType,
		))
	}
	return resultType
}

func (c *TypeChecker) checkUnaryOperation(unaryOp *ast.UnaryOperation) ast.Type {
	operandType := c.checkExpression(unaryOp.Operand)
	resultType, ok := unaryOperationResult(unaryOp.Operator, operandType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: unary operation %s cannot be applied to a value of type %s",
			unaryOp.Loc,
			unaryOp.Operator,
			operandType,
		))
	}
	return resultType
}

func (c *TypeChecker) checkIfStatement(stmt *ast.IfStatement) {
	exprType := c.checkExpression(stmt.Condition)
	if exprType != ast.Bool {
		c.errors = append(c.errors, fmt.Errorf("%s: expected an expression of type bool in if condition, got type %s",
			stmt.Loc,
			exprType,
		))
	}
	c.checkBlock(&stmt.ThenBlock)
	if stmt.ElseBlock != nil {
		c.checkBlock(stmt.ElseBlock)
	}
}

func (c *TypeChecker) checkWhileStatement(stmt *ast.WhileStatement) {
	exprType := c.checkExpression(stmt.Condition)
	if exprType != ast.Bool {
		c.errors = append(c.errors, fmt.Errorf("%s: expected an expression of type bool in while condition, got type %s",
			stmt.Loc,
			exprType,
		))
	}
	c.checkBlock(&stmt.Body)
}

func (c *TypeChecker) checkBreakStatement(stmt *ast.BreakStatement) {
	// noop
}

func (c *TypeChecker) checkContinueStatement(stmt *ast.ContinueStatement) {
	// noop
}

func (c *TypeChecker) checkFieldAccess(lvalue *ast.FieldLValue) ast.Type {
	// TODO: Check that Object is actually an lvalue!
	objectType := c.checkExpression(lvalue.Object)

	structType, ok := objectType.(*ast.BaseType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: type %s used in field access is not a base type", lvalue.Loc, objectType))
		return nil
	}

	structDesc, err := c.types.GetStruct(structType)
	if err != nil {
		c.errors = append(c.errors, fmt.Errorf("%s: %v", lvalue.Loc, err))
	}

	if structDesc == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: cannot access field %s of a non-struct", lvalue.Loc, lvalue.FieldName))
		return nil
	}
	fieldType := structDesc.GetField(lvalue.FieldName)
	if fieldType == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: struct %s does not have field %s", lvalue.Loc, structDesc.Name, lvalue.FieldName))
	}

	return fieldType
}

func binaryOperationResult(op string, left, right ast.Type) (ast.Type, bool) {
	if !left.Equals(right) {
		return nil, false
	}

	if op == "==" || op == "!=" {
		// Equality is supported for all types.
		return ast.Bool, true
	}

	if op == "+" || op == "-" || op == "/" || op == "*" || op == "%" {
		// These are (currently) supproted for integers only.
		return left, left == ast.Int || left == ast.Int64
	}

	if op == "<" || op == ">" || op == "<=" || op == ">=" {
		// These are (currently) supproted for integers only.
		return ast.Bool, left == ast.Int || left == ast.Int64
	}

	if op == "&&" || op == "||" {
		return ast.Bool, left == ast.Bool
	}

	panic(fmt.Sprintf("unknown binary operation %s", op))
}

func unaryOperationResult(op string, val ast.Type) (ast.Type, bool) {
	switch op {
	case "!":
		return ast.Bool, val == ast.Bool
	case "-":
		return val, val == ast.Int || val == ast.Int64
	case "&":
		// We can make a pointer to any type.
		// TODO: Check that we're only taking address of lvalues.
		return ast.NewPointerType(val), true
	case "*":
		if ptr, ok := val.(*ast.PointerType); ok {
			return ptr.ElementType, true
		} else {
			return nil, false
		}
	}
	panic(fmt.Sprintf("unknown unary operation %s", op))
}
