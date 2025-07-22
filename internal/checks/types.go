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
		t := c.checkLiteral(literal)
		literal.Type = t
		return t
	} else if assignment, ok := expr.(*ast.Assignment); ok {
		t := c.checkAssignment(assignment)
		assignment.Type = t
		return t
	} else if functionCall, ok := expr.(*ast.FunctionCall); ok {
		t := c.checkFunctionCall(functionCall)
		functionCall.Type = t
		return t
	} else if variableReference, ok := expr.(*ast.VariableReference); ok {
		t := c.checkVariableReference(variableReference)
		variableReference.Type = t
		return t
	} else if binaryOperation, ok := expr.(*ast.BinaryOperation); ok {
		t := c.checkBinaryOperation(binaryOperation)
		binaryOperation.Type = t
		return t
	} else if unaryOperation, ok := expr.(*ast.UnaryOperation); ok {
		t := c.checkUnaryOperation(unaryOperation)
		unaryOperation.Type = t
		return t
	} else if lvalue, ok := expr.(*ast.FieldLValue); ok {
		// TODO: Should we also mark nodes as lvalue/rvalue?
		t := c.checkFieldAccess(lvalue.Loc, lvalue.Object, lvalue.FieldName)
		lvalue.Type = t
		return t
	} else if fa, ok := expr.(*ast.FieldAccess); ok {
		t := c.checkFieldAccess(fa.Loc, fa.Object, fa.FieldName)
		fa.Type = t
		return t
	} else if newEx, ok := expr.(*ast.NewExpression); ok {
		t := c.checkNewExpression(newEx)
		newEx.Type = t
		return t
	}
	panic(fmt.Sprintf("Invalid expression type: %v", expr))
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
	} else if lit.NullValue {
		return ast.NullPtr
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
		// TODO: Check that the type is valid!

		if i >= len(proto.Args) {
			continue
		}

		expectedArgType := proto.Args[i].Typ
		if expectedArgType == ast.VoidPtr {
			if !ast.IsPointerType(actualArgType) {
				c.errors = append(c.errors, fmt.Errorf("%s: argument of %s must be a pointer, got %s", call.Loc, call.FunctionName, actualArgType))
			}
		} else if !actualArgType.Equals(expectedArgType) {
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
		targetType = c.checkFieldAccess(lvalue.Loc, lvalue.Object, lvalue.FieldName)
	} else {
		panic(fmt.Errorf("%s: invalid lvalue %s in assignment", assignment.Loc, assignment.Target))
	}

	valueType := c.checkExpression(assignment.Value)
	isValidNullAssignment := ast.IsPointerType(targetType) && valueType.Equals(ast.NullPtr)
	if !valueType.Equals(targetType) && !isValidNullAssignment {
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
	resultType, ok := c.unaryOperationResult(unaryOp.Operator, operandType)
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

func (c *TypeChecker) checkFieldAccess(loc ast.Location, object ast.Expression, fieldName string) ast.Type {
	objectType := c.checkExpression(object)

	if ptrType, ok := objectType.(*ast.PointerType); ok {
		// Auto-dereference structs in field access.
		objectType = ptrType.ElementType
	}

	structType, ok := objectType.(*ast.BaseType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: type %s used in field access is not a base type", loc, objectType))
		return nil
	}

	structDesc, err := c.types.GetStruct(structType)
	if err != nil {
		c.errors = append(c.errors, fmt.Errorf("%s: %v", loc, err))
		return nil
	}

	if structDesc == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: cannot access field %s of a non-struct", loc, fieldName))
		return nil
	}
	fieldType := structDesc.GetFieldType(fieldName)
	if fieldType == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: struct %s does not have field %s", loc, structDesc.Name, fieldName))
		return nil
	}

	return fieldType
}

func (c *TypeChecker) checkNewExpression(n *ast.NewExpression) ast.Type {
	// TODO: Check that TypeExpr is a valid type.
	return &ast.PointerType{ElementType: n.TypeExpr}
}

func (c *TypeChecker) unaryOperationResult(op string, val ast.Type) (ast.Type, bool) {
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

func binaryOperationResult(op string, left, right ast.Type) (ast.Type, bool) {
	if !areCompatibleTypes(left, right) {
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

func areCompatibleTypes(left, right ast.Type) bool {
	if left.Equals(right) {
		return true
	}

	if left == ast.NullPtr && ast.IsPointerType(right) || right == ast.NullPtr && ast.IsPointerType(left) {
		// null can be used with pointers of any type.
		return true
	}

	return false
}
