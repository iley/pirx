package typechecker

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/types"
)

// TypeChecker checks and fills in types in a program.
// It treats the input AST as effectively immutable and creates a full copy of the AST with the types filled in.
// Additionally it flattens nested variable scopes by giving each variable a name unique within the function.
type TypeChecker struct {
	program       *ast.Program
	vars          *varStack
	declaredFuncs map[string]types.FuncProto
	types         *types.TypeTable
	errors        []error
	currentFunc   types.FuncProto
	hasReturn     bool
}

func NewTypeChecker(program *ast.Program) *TypeChecker {
	return &TypeChecker{
		program:       program,
		declaredFuncs: make(map[string]types.FuncProto),
		errors:        []error{},
	}
}

func (c *TypeChecker) Errors() []error {
	return c.errors
}

func (c *TypeChecker) Check() *ast.Program {
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

	checkedFunctions := make([]ast.Function, len(c.program.Functions))
	for i, fn := range c.program.Functions {
		checkedFunctions[i] = c.checkFunction(fn)
	}

	typeDecls := make([]ast.TypeDeclaration, len(c.program.TypeDeclarations))
	copy(typeDecls, c.program.TypeDeclarations)

	return &ast.Program{
		Loc:              c.program.Loc,
		Functions:        checkedFunctions,
		TypeDeclarations: typeDecls,
	}
}

func (c *TypeChecker) checkFunction(fn ast.Function) ast.Function {
	c.currentFunc = c.declaredFuncs[fn.Name]
	c.hasReturn = false
	c.vars = newVarStack()

	// Create the root scope for the function.
	c.vars.startScope()

	for _, arg := range fn.Args {
		ok := c.vars.declare(arg.Name, arg.Type)
		if !ok {
			c.errors = append(c.errors, fmt.Errorf("%s: duplicate function argument: %s", fn.Loc, arg.Name))
		}
	}

	var checkedBody *ast.Block
	if fn.Body != nil {
		checkedBody = c.checkBlock(fn.Body)
	}

	// TODO: Check that each possible execution path ends with a return.
	if c.currentFunc.ReturnType != nil && !c.hasReturn {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s with return type %s must contain a return operator", fn.Loc, c.currentFunc.Name, c.currentFunc.ReturnType))
	}

	args := make([]ast.Arg, len(fn.Args))
	copy(args, fn.Args)

	return ast.Function{
		Loc:        fn.Loc,
		Name:       fn.Name,
		Args:       args,
		Body:       checkedBody,
		ReturnType: fn.ReturnType,
		External:   fn.External,
	}
}

func (c *TypeChecker) checkBlock(block *ast.Block) *ast.Block {
	checkedStatements := make([]ast.Statement, len(block.Statements))

	c.vars.startScope()
	for i, stmt := range block.Statements {
		checkedStatements[i] = c.checkStatement(stmt)
	}
	c.vars.endScope()

	return &ast.Block{
		Loc:        block.Loc,
		Statements: checkedStatements,
	}
}

func (c *TypeChecker) checkStatement(stmt ast.Statement) ast.Statement {
	if varDecl, ok := stmt.(*ast.VariableDeclaration); ok {
		return c.checkVariableDeclaration(varDecl)
	} else if exprStmt, ok := stmt.(*ast.ExpressionStatement); ok {
		return c.checkExpressionStatement(exprStmt)
	} else if retStmt, ok := stmt.(*ast.ReturnStatement); ok {
		return c.checkReturnStatement(retStmt)
	} else if ifStmt, ok := stmt.(*ast.IfStatement); ok {
		return c.checkIfStatement(ifStmt)
	} else if whileStmt, ok := stmt.(*ast.WhileStatement); ok {
		return c.checkWhileStatement(whileStmt)
	} else if breakStmt, ok := stmt.(*ast.BreakStatement); ok {
		return c.checkBreakStatement(breakStmt)
	} else if contStmt, ok := stmt.(*ast.ContinueStatement); ok {
		return c.checkContinueStatement(contStmt)
	} else if blockStmt, ok := stmt.(*ast.BlockStatement); ok {
		return c.checkBlockStatement(blockStmt)
	} else {
		panic(fmt.Sprintf("unsupported statement type: %v", stmt))
	}
}

func (c *TypeChecker) checkExpression(expr ast.Expression) ast.Expression {
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
		return c.checkFieldLValue(lvalue)
	} else if fa, ok := expr.(*ast.FieldAccess); ok {
		return c.checkFieldAccess(fa)
	} else if newEx, ok := expr.(*ast.NewExpression); ok {
		return c.checkNewExpression(newEx)
	}
	panic(fmt.Sprintf("Invalid expression type: %v", expr))
}

func (c *TypeChecker) checkLiteral(lit *ast.Literal) *ast.Literal {
	var t ast.Type
	if lit.StringValue != nil {
		t = ast.String
	} else if lit.IntValue != nil {
		t = ast.Int
	} else if lit.Int64Value != nil {
		t = ast.Int64
	} else if lit.Int8Value != nil {
		t = ast.Int8
	} else if lit.BoolValue != nil {
		t = ast.Bool
	} else if lit.NullValue {
		t = ast.NullPtr
	} else {
		panic(fmt.Sprintf("unknown literal type: %v", *lit))
	}

	result := *lit
	result.Type = t
	return &result
}

func (c *TypeChecker) checkVariableDeclaration(decl *ast.VariableDeclaration) *ast.VariableDeclaration {
	var checkedInitializer ast.Expression
	typ := decl.Type

	if decl.Initializer != nil {
		checkedInitializer = c.checkExpression(decl.Initializer)

		if typ != nil && !areCompatibleTypes(decl.Type, checkedInitializer.GetType()) {
			c.errors = append(c.errors, fmt.Errorf("%s: cannot initialize variable %s of type %s with expression of type %s",
				decl.Loc, decl.Name, decl.Type, checkedInitializer.GetType()))
		}
	}

	if typ == nil {
		if checkedInitializer != nil && checkedInitializer.GetType() != nil {
			// Type inference.
			typ = checkedInitializer.GetType()
		}
	}

	ok := c.vars.declare(decl.Name, typ)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: variable %s is already declared", decl.Loc, decl.Name))
	}

	_, uniqueName := c.vars.lookup(decl.Name)

	result := *decl
	result.Initializer = checkedInitializer
	result.Name = uniqueName
	result.Type = typ
	return &result
}

func (c *TypeChecker) checkFunctionCall(call *ast.FunctionCall) *ast.FunctionCall {
	proto, declared := c.declaredFuncs[call.FunctionName]
	if !declared {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s is not declared", call.Loc, call.FunctionName))
		// Use a default proto to avoid nil reference issues
		proto.ReturnType = ast.Int
	}

	if declared && !proto.Variadic && (len(proto.Args) != len(call.Args)) {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s has %d arguments but %d were provided", call.Loc, call.FunctionName, len(proto.Args), len(call.Args)))
	}

	checkedArgs := make([]ast.Expression, len(call.Args))
	for i, expr := range call.Args {
		checkedArgs[i] = c.checkExpression(expr)
		actualArgType := checkedArgs[i].GetType()
		// TODO: Check that the type is valid!

		if !declared || i >= len(proto.Args) {
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

	return &ast.FunctionCall{
		Loc:          call.Loc,
		FunctionName: call.FunctionName,
		Args:         checkedArgs,
		Variadic:     call.Variadic,
		Type:         proto.ReturnType,
	}
}

func (c *TypeChecker) checkExpressionStatement(e *ast.ExpressionStatement) *ast.ExpressionStatement {
	result := *e
	result.Expression = c.checkExpression(e.Expression)
	return &result
}

func (c *TypeChecker) checkAssignment(assignment *ast.Assignment) *ast.Assignment {
	checkedTarget := c.checkLValue(assignment.Target)
	targetType := checkedTarget.GetType()

	checkedValue := c.checkExpression(assignment.Value)
	valueType := checkedValue.GetType()

	if valueType != nil && targetType != nil {
		isValidNullAssignment := ast.IsPointerType(targetType) && valueType.Equals(ast.NullPtr)
		if !valueType.Equals(targetType) && !isValidNullAssignment {
			c.errors = append(c.errors, fmt.Errorf("%s: cannot assign value of type %s to lvalue of type %s",
				assignment.Loc,
				valueType,
				targetType,
			))
		}
	}

	// Ensure we always have a valid type
	if targetType == nil {
		targetType = ast.Int
	}

	result := *assignment
	result.Target = checkedTarget
	result.Value = checkedValue
	result.Type = targetType
	return &result
}

func (c *TypeChecker) checkLValue(lval ast.LValue) ast.LValue {
	switch l := lval.(type) {
	case *ast.VariableLValue:
		return c.checkVariableLValue(l)
	case *ast.DereferenceLValue:
		return c.checkDereferenceLValue(l)
	case *ast.FieldLValue:
		return c.checkFieldLValue(l)
	default:
		panic(fmt.Errorf("invalid lvalue type: %T", lval))
	}
}

func (c *TypeChecker) checkVariableLValue(lval *ast.VariableLValue) *ast.VariableLValue {
	typ, uniqueName := c.vars.lookup(lval.Name)
	if typ == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: variable %s is not declared before assignment", lval.Loc, lval.Name))
		typ = ast.Undefined // Use a default type to avoid nil
	}
	result := *lval
	result.Name = uniqueName
	result.Type = typ
	return &result
}

func (c *TypeChecker) checkDereferenceLValue(lval *ast.DereferenceLValue) *ast.DereferenceLValue {
	exprChecked := c.checkExpression(lval.Expression)
	refType := exprChecked.GetType()
	ptrType, ok := refType.(*ast.PointerType)
	var resultType ast.Type
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: dereference of a non-pointer type %s", lval.Loc, refType))
		resultType = ast.Int // Use a default type to avoid nil
	} else {
		resultType = ptrType.ElementType
	}
	result := *lval
	result.Expression = exprChecked
	result.Type = resultType
	return &result
}

func (c *TypeChecker) checkVariableReference(ref *ast.VariableReference) *ast.VariableReference {
	typ, uniqueName := c.vars.lookup(ref.Name)
	if typ == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: variable %s is not declared before reference", ref.Loc, ref.Name))
		typ = ast.Undefined // Use a default type to avoid nil
	}

	result := *ref
	result.Name = uniqueName
	result.Type = typ
	return &result
}

func (c *TypeChecker) checkReturnStatement(stmt *ast.ReturnStatement) *ast.ReturnStatement {
	c.hasReturn = true

	if stmt.Value == nil && c.currentFunc.ReturnType != nil {
		c.errors = append(c.errors, fmt.Errorf("%s: function %s should return a value of type %s but no value was provided",
			stmt.Loc, c.currentFunc.Name, c.currentFunc.ReturnType,
		))
	}

	var checkedValue ast.Expression
	if stmt.Value != nil {
		checkedValue = c.checkExpression(stmt.Value)
		typ := checkedValue.GetType()
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

	result := *stmt
	result.Value = checkedValue
	return &result
}

func (c *TypeChecker) checkBinaryOperation(binOp *ast.BinaryOperation) *ast.BinaryOperation {
	leftExpr := c.checkExpression(binOp.Left)
	rightExpr := c.checkExpression(binOp.Right)
	leftType := leftExpr.GetType()
	rightType := rightExpr.GetType()
	resultType, ok := binaryOperationResult(binOp.Operator, leftType, rightType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: binary operation %s cannot be applied to values of types %s and %s",
			binOp.Loc,
			binOp.Operator,
			leftType,
			rightType,
		))
		resultType = ast.Int // Use a default type to avoid nil
	}
	result := *binOp
	result.Left = leftExpr
	result.Right = rightExpr
	result.Type = resultType
	return &result
}

func (c *TypeChecker) checkUnaryOperation(unaryOp *ast.UnaryOperation) *ast.UnaryOperation {
	operandExpr := c.checkExpression(unaryOp.Operand)
	operandType := operandExpr.GetType()
	resultType, ok := c.unaryOperationResult(unaryOp.Operator, operandType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: unary operation %s cannot be applied to a value of type %s",
			unaryOp.Loc,
			unaryOp.Operator,
			operandType,
		))
		resultType = ast.Int // Use a default type to avoid nil
	}
	result := *unaryOp
	result.Operand = operandExpr
	result.Type = resultType
	return &result
}

func (c *TypeChecker) checkIfStatement(stmt *ast.IfStatement) *ast.IfStatement {
	checkedCondition := c.checkExpression(stmt.Condition)
	exprType := checkedCondition.GetType()
	if exprType != ast.Bool {
		c.errors = append(c.errors, fmt.Errorf("%s: expected an expression of type bool in if condition, got type %s",
			stmt.Loc,
			exprType,
		))
	}
	checkedThenBlock := c.checkBlock(&stmt.ThenBlock)
	var checkedElseBlock *ast.Block
	if stmt.ElseBlock != nil {
		checkedElseBlock = c.checkBlock(stmt.ElseBlock)
	}
	return &ast.IfStatement{
		Loc:       stmt.Loc,
		Condition: checkedCondition,
		ThenBlock: *checkedThenBlock,
		ElseBlock: checkedElseBlock,
	}
}

func (c *TypeChecker) checkWhileStatement(stmt *ast.WhileStatement) *ast.WhileStatement {
	checkedCondition := c.checkExpression(stmt.Condition)
	exprType := checkedCondition.GetType()
	if exprType != ast.Bool {
		c.errors = append(c.errors, fmt.Errorf("%s: expected an expression of type bool in while condition, got type %s",
			stmt.Loc,
			exprType,
		))
	}
	checkedBody := c.checkBlock(&stmt.Body)
	return &ast.WhileStatement{
		Loc:       stmt.Loc,
		Condition: checkedCondition,
		Body:      *checkedBody,
	}
}

func (c *TypeChecker) checkBreakStatement(stmt *ast.BreakStatement) *ast.BreakStatement {
	return &ast.BreakStatement{Loc: stmt.Loc}
}

func (c *TypeChecker) checkContinueStatement(stmt *ast.ContinueStatement) *ast.ContinueStatement {
	return &ast.ContinueStatement{Loc: stmt.Loc}
}

func (c *TypeChecker) checkBlockStatement(stmt *ast.BlockStatement) *ast.BlockStatement {
	checkedBlock := c.checkBlock(&stmt.Block)
	return &ast.BlockStatement{
		Loc:   stmt.Loc,
		Block: *checkedBlock,
	}
}

func (c *TypeChecker) checkFieldAccess(fa *ast.FieldAccess) *ast.FieldAccess {
	objectExpr := c.checkExpression(fa.Object)
	fieldType := c.getFieldType(fa.Loc, objectExpr, fa.FieldName)
	result := *fa
	result.Object = objectExpr
	result.Type = fieldType
	return &result
}

func (c *TypeChecker) checkFieldLValue(lvalue *ast.FieldLValue) *ast.FieldLValue {
	objectExpr := c.checkExpression(lvalue.Object)
	fieldType := c.getFieldType(lvalue.Loc, objectExpr, lvalue.FieldName)
	result := *lvalue
	result.Object = objectExpr
	result.Type = fieldType
	return &result
}

func (c *TypeChecker) getFieldType(loc ast.Location, objectExpr ast.Expression, fieldName string) ast.Type {
	objectType := objectExpr.GetType()

	if ptrType, ok := objectType.(*ast.PointerType); ok {
		// Auto-dereference structs in field access.
		objectType = ptrType.ElementType
	}

	structType, ok := objectType.(*ast.BaseType)
	if !ok {
		c.errors = append(c.errors, fmt.Errorf("%s: type %s used in field access is not a base type", loc, objectType))
		return ast.Int // Use a default type to avoid nil
	}

	structDesc, err := c.types.GetStruct(structType)
	if err != nil {
		c.errors = append(c.errors, fmt.Errorf("%s: %v", loc, err))
		return ast.Int // Use a default type to avoid nil
	}

	if structDesc == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: cannot access field %s of a non-struct", loc, fieldName))
		return ast.Int // Use a default type to avoid nil
	}
	fieldType := structDesc.GetFieldType(fieldName)
	if fieldType == nil {
		c.errors = append(c.errors, fmt.Errorf("%s: struct %s does not have field %s", loc, structDesc.Name, fieldName))
		return ast.Int // Use a default type to avoid nil
	}

	return fieldType
}

func (c *TypeChecker) checkNewExpression(n *ast.NewExpression) *ast.NewExpression {
	// TODO: Check that TypeExpr is a valid type.
	result := *n
	result.Type = &ast.PointerType{ElementType: n.TypeExpr}
	return &result
}

func (c *TypeChecker) unaryOperationResult(op string, val ast.Type) (ast.Type, bool) {
	switch op {
	case "!":
		return ast.Bool, val == ast.Bool
	case "-":
		return val, ast.IsIntegerType(val)
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
		return left, ast.IsIntegerType(left)
	}

	if op == "<" || op == ">" || op == "<=" || op == ">=" {
		// These are (currently) supproted for integers only.
		return ast.Bool, ast.IsIntegerType(left)
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
