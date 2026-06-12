package typechecker

import (
	"fmt"
	"strings"

	"github.com/iley/pirx/internal/ast"
)

// TypeChecker checks and fills in types in a program.
// It treats the input AST as effectively immutable and creates a full copy of the AST with the types filled in.
// Additionally it flattens nested variable scopes by giving each variable a name unique within the function.
type TypeChecker struct {
	program       *ast.Program
	vars          *varStack
	declaredFuncs map[string]ast.FuncProto
	types         *ast.TypeTable
	errors        []error
	currentFunc   ast.FuncProto
	nestedLoops   int
}

func NewTypeChecker(program *ast.Program) *TypeChecker {
	return &TypeChecker{
		program:       program,
		declaredFuncs: make(map[string]ast.FuncProto),
		vars:          newVarStack(),
		errors:        []error{},
	}
}

func (c *TypeChecker) Check() (*ast.Program, []error) {
	// Gather function prototypes so we can check arguments and types later.
	protos := ast.GetFunctionTable(c.program)
	for _, proto := range protos {
		c.declaredFuncs[proto.Name] = proto
	}

	c.checkDuplicateFunctions()

	types, err := ast.MakeTypeTable(c.program.TypeDeclarations)
	if err != nil {
		c.errors = append(c.errors, err)
		return &ast.Program{}, c.errors
	}
	c.types = types

	// MakeTypeTable validates field types it needs the size of, but types behind a pointer
	// (or a slice element) are never sized, so validate all field types explicitly.
	for _, decl := range c.program.TypeDeclarations {
		if structDecl, ok := decl.(*ast.StructDeclaration); ok {
			for _, field := range structDecl.Fields {
				c.validateType(field.Loc, field.Type)
			}
		}
	}

	c.vars.startScope()

	checkedVarDecls := make([]ast.VariableDeclaration, len(c.program.VariableDeclarations))
	for i, decl := range c.program.VariableDeclarations {
		checkedVarDecls[i] = *c.checkVariableDeclaration(&decl /*global=*/, true)
	}

	checkedFunctions := make([]ast.Function, len(c.program.Functions))
	for i, fn := range c.program.Functions {
		checkedFunctions[i] = c.checkFunction(fn)
	}

	typeDecls := make([]ast.TypeDeclaration, len(c.program.TypeDeclarations))
	copy(typeDecls, c.program.TypeDeclarations)

	return &ast.Program{
		Loc:                  c.program.Loc,
		Functions:            checkedFunctions,
		TypeDeclarations:     typeDecls,
		VariableDeclarations: checkedVarDecls,
		TypeTable:            types,
	}, c.errors
}

func (c *TypeChecker) checkDuplicateFunctions() {
	builtins := make(map[string]bool)
	declared := make(map[string]ast.FuncProto)
	for _, proto := range ast.GetBuiltins() {
		builtins[proto.Name] = true
		declared[proto.Name] = proto
	}

	defined := make(map[string]bool)
	for _, fn := range c.program.Functions {
		if fn.Body != nil {
			if builtins[fn.Name] {
				c.errorf("%s: function %s conflicts with a builtin function", fn.Loc, fn.Name)
				continue
			}
			if defined[fn.Name] {
				c.errorf("%s: function %s is already defined", fn.Loc, fn.Name)
				continue
			}
			defined[fn.Name] = true
		}

		// A function can be declared multiple times (e.g. re-declaring a builtin as an extern,
		// or a declaration followed by the definition), but the signatures must agree.
		proto := ast.MakeFuncProto(fn)
		if prev, ok := declared[fn.Name]; ok {
			if !proto.SameSignature(prev) {
				c.errorf("%s: declaration of function %s conflicts with a previous declaration", fn.Loc, fn.Name)
			}
		} else {
			declared[fn.Name] = proto
		}
	}
}

// validateType reports an error if the type (or any type it is built from) is not declared.
func (c *TypeChecker) validateType(loc ast.Location, typ ast.Type) {
	switch t := typ.(type) {
	case *ast.PointerType:
		c.validateType(loc, t.ElementType)
	case *ast.SliceType:
		c.validateType(loc, t.ElementType)
	case *ast.BaseType:
		if !c.types.HasType(t.Name) {
			c.errorf("%s: unknown type %s", loc, t.Name)
		}
	}
}

func (c *TypeChecker) checkFunction(fn ast.Function) ast.Function {
	c.currentFunc = c.declaredFuncs[fn.Name]
	c.nestedLoops = 0

	c.vars.resetUsageCounts()
	c.vars.startScope()
	defer c.vars.endScope()

	for _, arg := range fn.Args {
		c.validateType(arg.Loc, arg.Type)
		// The code generators don't implement the C ABI rules for aggregates with
		// floating-point members (SSE-class eightbytes on x86_64, HFAs on arm64),
		// so reject them in extern signatures rather than pass garbage at runtime.
		if fn.External && c.isFloatStruct(arg.Type) {
			c.errorf("%s: argument %s of extern function %s: structs with floating-point fields are not supported in extern functions", fn.Loc, arg.Name, fn.Name)
		}
		ok := c.vars.declare(arg.Name, arg.Type /*global=*/, false /*constant=*/, false)
		if !ok {
			c.errorf("%s: duplicate function argument: %s", fn.Loc, arg.Name)
		}
	}

	if fn.ReturnType != nil {
		c.validateType(fn.Loc, fn.ReturnType)
		if fn.External && c.isFloatStruct(fn.ReturnType) {
			c.errorf("%s: return type of extern function %s: structs with floating-point fields are not supported in extern functions", fn.Loc, fn.Name)
		}
	}

	var checkedBody *ast.Block
	if fn.Body != nil {
		checkedBody = c.checkBlock(fn.Body)

		if c.currentFunc.ReturnType != nil && !blockReturns(checkedBody) {
			c.errorf("%s: function %s with return type %s must return a value on all code paths", fn.Loc, c.currentFunc.Name, c.currentFunc.ReturnType)
		}
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

// isFloatStruct reports whether typ is a struct (passed by value) with a
// floating-point field, directly or in a nested struct.
func (c *TypeChecker) isFloatStruct(typ ast.Type) bool {
	sd, err := c.types.GetStruct(typ)
	if err != nil {
		return false
	}
	for _, field := range sd.Fields {
		if ast.IsFloatingPointType(field.Type) || c.isFloatStruct(field.Type) {
			return true
		}
	}
	return false
}

func (c *TypeChecker) checkBlock(block *ast.Block) *ast.Block {
	checkedStatements := make([]ast.Statement, len(block.Statements))

	c.vars.startScope()
	defer c.vars.endScope()

	for i, stmt := range block.Statements {
		checkedStatements[i] = c.checkStatement(stmt)
	}

	return &ast.Block{
		Loc:        block.Loc,
		Statements: checkedStatements,
	}
}

func (c *TypeChecker) checkStatement(stmt ast.Statement) ast.Statement {
	if varDecl, ok := stmt.(*ast.VariableDeclaration); ok {
		return c.checkVariableDeclaration(varDecl /*global=*/, false)
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
	} else if forLoop, ok := stmt.(*ast.ForStatement); ok {
		return c.checkForLoop(forLoop)
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
	} else if fa, ok := expr.(*ast.FieldAccess); ok {
		return c.checkFieldAccess(fa)
	} else if indexExpr, ok := expr.(*ast.IndexExpression); ok {
		return c.checkIndexExpression(indexExpr)
	} else if rangeExpr, ok := expr.(*ast.RangeExpression); ok {
		return c.checkRangeExpression(rangeExpr)
	} else if newEx, ok := expr.(*ast.NewExpression); ok {
		return c.checkNewExpression(newEx)
	} else if po, ok := expr.(*ast.PostfixOperator); ok {
		return c.checkPostfixOperator(po)
	} else if pre, ok := expr.(*ast.PrefixOperator); ok {
		return c.checkPrefixOperator(pre)
	} else if il, ok := expr.(*ast.InitializerList); ok {
		// Initializer lists have no type of their own, so they can only appear where
		// the expected type is known from the context. See checkInitializerList.
		c.errorf("%s: initializer lists are only allowed in variable declarations", il.Loc)
		result := *il
		result.Type = ast.Undefined
		return &result
	}
	panic(fmt.Sprintf("Invalid expression type: %v", expr))
}

// checkValueExpression checks an expression that is required to produce a value,
// e.g. an operand, an initializer or a call argument.
func (c *TypeChecker) checkValueExpression(expr ast.Expression) ast.Expression {
	checked := c.checkExpression(expr)
	if checked.GetType() == nil {
		// The only expression without a type is a call of a function that doesn't return anything.
		if call, ok := checked.(*ast.FunctionCall); ok {
			c.errorf("%s: function %s does not return a value", call.Loc, call.FunctionName)
			call.Type = ast.Undefined
		}
	}
	return checked
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
	} else if lit.Float32Value != nil {
		t = ast.Float32
	} else if lit.Float64Value != nil {
		t = ast.Float64
	} else if lit.NullValue {
		t = ast.NullPtr
	} else {
		panic(fmt.Sprintf("unknown literal type: %v", *lit))
	}

	result := *lit
	result.Type = t
	return &result
}

func (c *TypeChecker) checkVariableDeclaration(decl *ast.VariableDeclaration, global bool) *ast.VariableDeclaration {
	var checkedInitializer ast.Expression
	typ := decl.Type

	if typ != nil {
		c.validateType(decl.Loc, typ)
	}

	if il, ok := decl.Initializer.(*ast.InitializerList); ok {
		// Initializer lists take their type from the declaration, so the type annotation is required.
		if typ == nil {
			c.errorf("%s: cannot infer the type of variable %s from an initializer list, type annotation required",
				decl.Loc, decl.Name)
			typ = ast.Undefined
		} else {
			checkedInitializer = c.checkInitializerList(il, typ)
		}
	} else if decl.Initializer != nil {
		checkedInitializer = c.checkValueExpression(decl.Initializer)

		if typ != nil && !areCompatibleTypes(decl.Type, checkedInitializer.GetType()) {
			c.errorf("%s: cannot initialize variable %s of type %s with expression of type %s",
				decl.Loc, decl.Name, decl.Type, checkedInitializer.GetType())
		}
	}

	if typ == nil {
		if checkedInitializer != nil && checkedInitializer.GetType() != nil {
			// Type inference.
			typ = checkedInitializer.GetType()
			if !ast.IsConcreteType(typ) {
				c.errorf("%s: invalid initializer for variable %s, cannot infer type", decl.Loc, decl.Name)
			}
		}
	}

	ok := c.vars.declare(decl.Name, typ, global, decl.IsConstant)
	if !ok {
		c.errorf("%s: variable %s is already declared", decl.Loc, decl.Name)
	}

	varDesc, ok := c.vars.lookup(decl.Name)
	if !ok {
		panic("variable lookup failed after declaration")
	}

	result := *decl
	result.Initializer = checkedInitializer
	result.Name = varDesc.uniqueName
	result.Type = typ
	return &result
}

// checkInitializerList checks an initializer list against the expected type that comes
// from the context (the variable declaration's type annotation). The elements are matched
// positionally against the struct's fields.
func (c *TypeChecker) checkInitializerList(il *ast.InitializerList, expected ast.Type) *ast.InitializerList {
	result := *il
	result.Type = expected

	structDesc, err := c.types.GetStruct(expected)
	if err != nil {
		c.errorf("%s: cannot initialize a value of type %s with an initializer list, only structs are supported",
			il.Loc, expected)
		result.Type = ast.Undefined
		return &result
	}

	if len(il.Elements) != len(structDesc.Fields) {
		c.errorf("%s: struct %s has %d fields but the initializer list has %d elements",
			il.Loc, structDesc.Name, len(structDesc.Fields), len(il.Elements))
		result.Type = ast.Undefined
		return &result
	}

	checkedElements := make([]ast.Expression, len(il.Elements))
	for i, elem := range il.Elements {
		field := structDesc.Fields[i]
		if nested, ok := elem.(*ast.InitializerList); ok {
			checkedElements[i] = c.checkInitializerList(nested, field.Type)
			continue
		}
		checked := c.checkValueExpression(elem)
		if !areCompatibleTypes(field.Type, checked.GetType()) {
			c.errorf("%s: cannot initialize field %s of type %s with expression of type %s",
				elem.GetLocation(), field.Name, field.Type, checked.GetType())
		}
		checkedElements[i] = checked
	}
	result.Elements = checkedElements
	return &result
}

func resolveSpecialFunctionReturnType(functionName string, protoReturnType ast.Type, args []ast.Expression) ast.Type {
	if functionName == "getptr" && len(args) >= 1 {
		firstArgType := args[0].GetType()
		if sliceType, ok := firstArgType.(*ast.SliceType); ok {
			return &ast.PointerType{ElementType: sliceType.ElementType}
		}
	}
	if functionName == "range" && len(args) >= 1 {
		firstArgType := args[0].GetType()
		if sliceType, ok := firstArgType.(*ast.SliceType); ok {
			return sliceType
		} else if firstArgType == ast.String {
			return ast.String
		}
	}
	return protoReturnType
}

func (c *TypeChecker) checkFunctionCall(call *ast.FunctionCall) *ast.FunctionCall {
	proto, declared := c.declaredFuncs[call.FunctionName]
	if !declared {
		c.errorf("%s: function %s is not declared", call.Loc, call.FunctionName)
		// Use a default proto to avoid nil reference issues
		proto.ReturnType = ast.Int
	}

	if declared && !proto.Variadic && (len(proto.Args) != len(call.Args)) {
		c.errorf("%s: function %s has %d arguments but %d were provided", call.Loc, call.FunctionName, len(proto.Args), len(call.Args))
	}

	// Variadic functions still require all of the declared (non-variadic) arguments.
	if declared && proto.Variadic && len(call.Args) < len(proto.Args) {
		c.errorf("%s: function %s requires at least %d arguments but %d were provided", call.Loc, call.FunctionName, len(proto.Args), len(call.Args))
	}

	checkedArgs := make([]ast.Expression, len(call.Args))
	for i, expr := range call.Args {
		checkedArgs[i] = c.checkValueExpression(expr)
		actualArgType := checkedArgs[i].GetType()

		if !declared {
			continue
		}

		if i >= len(proto.Args) {
			// Variadic arguments go straight into a C call; see the float-struct
			// restriction on extern signatures in checkFunction.
			if c.isFloatStruct(actualArgType) {
				c.errorf("%s: argument #%d of function %s: structs with floating-point fields cannot be passed to extern functions", call.Loc, i+1, call.FunctionName)
			}
			continue
		}

		expectedArgType := proto.Args[i].Typ
		if expectedArgType == ast.VoidPtr {
			if !ast.IsPointerType(actualArgType) {
				c.errorf("%s: argument of %s must be a pointer, got %s", call.Loc, call.FunctionName, actualArgType)
			}
		} else if expectedArgType == ast.Disposable {
			if !ast.IsPointerType(actualArgType) && !ast.IsSliceType(actualArgType) && actualArgType != ast.String {
				c.errorf("%s: argument of %s must be either a pointer or a slice, got %s", call.Loc, call.FunctionName, actualArgType)
			}
		} else if expectedArgType == ast.Any {
			// Accept any type.
		} else if expectedArgType == ast.AnySlice {
			if !ast.IsSliceType(actualArgType) && actualArgType != ast.String {
				c.errorf("%s: expected a list type, got %s", call.Loc, actualArgType)
			}
		} else if expectedArgType == ast.AnySlicePtr {
			if !ast.IsSlicePointerType(actualArgType) {
				c.errorf("%s argument of %s must be a pointer to a slice, got %s", call.Loc, call.FunctionName, actualArgType)
			}
		} else if expectedArgType == ast.Numeric {
			if !ast.IsNumericType(actualArgType) {
				c.errorf("%s argument of %s must be of a numeric type, got %s", call.Loc, call.FunctionName, actualArgType)
			}
		} else if !actualArgType.Equals(expectedArgType) {
			c.errorf("%s: argument #%d of function %s has wrong type: expected %s but got %s",
				call.Loc, i+1, call.FunctionName, expectedArgType, actualArgType)
		}
	}

	returnType := resolveSpecialFunctionReturnType(call.FunctionName, proto.ReturnType, checkedArgs)

	return &ast.FunctionCall{
		Loc:          call.Loc,
		FunctionName: call.FunctionName,
		Args:         checkedArgs,
		Variadic:     call.Variadic,
		Type:         returnType,
	}
}

func (c *TypeChecker) checkExpressionStatement(e *ast.ExpressionStatement) *ast.ExpressionStatement {
	result := *e
	result.Expression = c.checkExpression(e.Expression)
	return &result
}

func (c *TypeChecker) checkAssignment(assignment *ast.Assignment) *ast.Assignment {
	checkedTarget := c.checkValueExpression(assignment.Target)
	targetType := checkedTarget.GetType()

	checkedValue := c.checkValueExpression(assignment.Value)
	valueType := checkedValue.GetType()

	if !isAddressable(checkedTarget) {
		c.errorf("%s: invalid assignment target", assignment.Loc)
	}

	if valueType != nil && targetType != nil {
		isValidNullAssignment := ast.IsPointerType(targetType) && valueType.Equals(ast.NullPtr)
		if !valueType.Equals(targetType) && !isValidNullAssignment {
			c.errorf("%s: cannot assign value of type %s to lvalue of type %s",
				assignment.Loc,
				valueType,
				targetType,
			)
		} else if assignment.Operator != "" && assignment.Operator != "=" {
			// A compound assignment is only valid if the underlying binary operation is.
			binaryOperator := strings.TrimSuffix(assignment.Operator, "=")
			if _, ok := c.binaryOperationResult(binaryOperator, targetType, valueType); !ok {
				c.errorf("%s: binary operation %s cannot be applied to values of types %s and %s",
					assignment.Loc, binaryOperator, targetType, valueType)
			}
		}
	}

	if name, isConst := c.constantRoot(assignment.Target, checkedTarget); isConst {
		c.errorf("%s: cannot assign value to constant %s", assignment.Loc, name)
	}

	// Ensure we always have a valid type to avoid nil dereference.
	if targetType == nil {
		targetType = ast.Undefined
	}

	result := *assignment
	result.Target = checkedTarget
	result.Value = checkedValue
	result.Type = targetType
	return &result
}

// isAddressable reports whether an expression denotes a memory location that can be written to
// or have its address taken. Notably, field access only produces a location when its base does
// (or when the base is a pointer): f().x is not assignable when f returns a struct by value.
func isAddressable(expr ast.Expression) bool {
	switch e := expr.(type) {
	case *ast.VariableReference:
		return true
	case *ast.UnaryOperation:
		return e.Operator == "*"
	case *ast.IndexExpression:
		// Slice elements live behind the slice's data pointer, so even an rvalue slice is fine.
		return true
	case *ast.FieldAccess:
		return ast.IsPointerType(e.Object.GetType()) || isAddressable(e.Object)
	default:
		return false
	}
}

// constantRoot reports whether an expression refers to the storage of a variable declared
// with val, and returns that variable's name. It walks field-access chains down to the root
// variable; dereference, indexing and pointer auto-dereference break the chain because the
// resulting location lives behind a pointer rather than in the constant's own storage.
// It takes both the original expression (checkExpression renames variables, and renamed
// variables can no longer be looked up by name) and the checked one (for type information).
func (c *TypeChecker) constantRoot(orig, checked ast.Expression) (string, bool) {
	switch origExpr := orig.(type) {
	case *ast.VariableReference:
		if vd, ok := c.vars.lookup(origExpr.Name); ok && vd.constant {
			return origExpr.Name, true
		}
	case *ast.FieldAccess:
		checkedFa, ok := checked.(*ast.FieldAccess)
		if ok && !ast.IsPointerType(checkedFa.Object.GetType()) {
			return c.constantRoot(origExpr.Object, checkedFa.Object)
		}
	}
	return "", false
}

func (c *TypeChecker) checkVariableReference(ref *ast.VariableReference) *ast.VariableReference {
	vd, ok := c.vars.lookup(ref.Name)
	if !ok {
		c.errorf("%s: variable %s is not declared before reference", ref.Loc, ref.Name)
		vd.typ = ast.Undefined
		vd.uniqueName = ref.Name
	}

	result := *ref
	result.Name = vd.uniqueName
	result.Type = vd.typ
	return &result
}

func (c *TypeChecker) checkReturnStatement(stmt *ast.ReturnStatement) *ast.ReturnStatement {
	if stmt.Value == nil && c.currentFunc.ReturnType != nil {
		c.errorf("%s: function %s should return a value of type %s but no value was provided",
			stmt.Loc, c.currentFunc.Name, c.currentFunc.ReturnType,
		)
	}

	var checkedValue ast.Expression
	if stmt.Value != nil {
		checkedValue = c.checkValueExpression(stmt.Value)
		typ := checkedValue.GetType()
		if c.currentFunc.ReturnType == nil {
			c.errorf("%s: function %s does not have a return type but a value was provided",
				stmt.Loc, c.currentFunc.Name,
			)
		} else if !areCompatibleTypes(typ, c.currentFunc.ReturnType) {
			c.errorf("%s: function %s has return type %s but a value of type %s was provided",
				stmt.Loc, c.currentFunc.Name, c.currentFunc.ReturnType, typ,
			)
		}
	}

	result := *stmt
	result.Value = checkedValue
	return &result
}

func (c *TypeChecker) checkBinaryOperation(binOp *ast.BinaryOperation) *ast.BinaryOperation {
	leftExpr := c.checkValueExpression(binOp.Left)
	rightExpr := c.checkValueExpression(binOp.Right)
	leftType := leftExpr.GetType()
	rightType := rightExpr.GetType()
	resultType, ok := c.binaryOperationResult(binOp.Operator, leftType, rightType)
	if !ok {
		c.errorf("%s: binary operation %s cannot be applied to values of types %s and %s",
			binOp.Loc,
			binOp.Operator,
			leftType,
			rightType,
		)
		resultType = ast.Int // Use a default type to avoid nil
	}
	result := *binOp
	result.Left = leftExpr
	result.Right = rightExpr
	result.Type = resultType
	return &result
}

func (c *TypeChecker) checkUnaryOperation(unaryOp *ast.UnaryOperation) *ast.UnaryOperation {
	operandExpr := c.checkValueExpression(unaryOp.Operand)
	operandType := operandExpr.GetType()
	// Addressability is a property of the expression, not its type, so it cannot
	// be checked in unaryOperationResult.
	if unaryOp.Operator == "&" {
		if !isAddressable(operandExpr) {
			c.errorf("%s: cannot take the address of an expression that is not an lvalue", unaryOp.Loc)
		}
		// A pointer to a constant's storage would allow mutating it, so reject it outright.
		if name, isConst := c.constantRoot(unaryOp.Operand, operandExpr); isConst {
			c.errorf("%s: cannot take the address of constant %s", unaryOp.Loc, name)
		}
	}
	resultType, ok := c.unaryOperationResult(unaryOp.Operator, operandType)
	if !ok {
		c.errorf("%s: unary operation %s cannot be applied to a value of type %s",
			unaryOp.Loc,
			unaryOp.Operator,
			operandType,
		)
		resultType = ast.Int // Use a default type to avoid nil
	}
	result := *unaryOp
	result.Operand = operandExpr
	result.Type = resultType
	return &result
}

func (c *TypeChecker) checkIfStatement(stmt *ast.IfStatement) *ast.IfStatement {
	checkedCondition := c.checkValueExpression(stmt.Condition)
	exprType := checkedCondition.GetType()
	if exprType != ast.Bool {
		c.errorf("%s: expected an expression of type bool in if condition, got type %s",
			stmt.Loc,
			exprType,
		)
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
	checkedCondition := c.checkValueExpression(stmt.Condition)
	exprType := checkedCondition.GetType()
	if exprType != ast.Bool {
		c.errorf("%s: expected an expression of type bool in while condition, got type %s",
			stmt.Loc,
			exprType,
		)
	}

	c.nestedLoops += 1
	checkedBody := c.checkBlock(&stmt.Body)
	c.nestedLoops -= 1

	return &ast.WhileStatement{
		Loc:       stmt.Loc,
		Condition: checkedCondition,
		Body:      *checkedBody,
	}
}

func (c *TypeChecker) checkForLoop(forLoop *ast.ForStatement) *ast.ForStatement {
	// Create a scope for the loop variable.
	c.vars.startScope()
	defer c.vars.endScope()

	var checkedInit ast.Statement
	if forLoop.Init != nil {
		checkedInit = c.checkStatement(forLoop.Init)
	}

	var checkedCond ast.Expression
	if forLoop.Condition != nil {
		checkedCond = c.checkValueExpression(forLoop.Condition)
		condType := checkedCond.GetType()
		if condType != ast.Bool {
			c.errorf("%s: expected an expression of type bool in for condition, got type %s",
				forLoop.Loc, condType)
		}
	}

	var checkedUpdate ast.Expression
	if forLoop.Update != nil {
		checkedUpdate = c.checkExpression(forLoop.Update)
	}

	c.nestedLoops++
	checkedBody := c.checkBlock(&forLoop.Body)
	c.nestedLoops--
	return &ast.ForStatement{
		Loc:       forLoop.Loc,
		Init:      checkedInit,
		Condition: checkedCond,
		Update:    checkedUpdate,
		Body:      *checkedBody,
	}
}

func (c *TypeChecker) checkBreakStatement(stmt *ast.BreakStatement) *ast.BreakStatement {
	if c.nestedLoops == 0 {
		c.errorf("%s: break cannot be used outside a loop", stmt.Loc)
	}
	return &ast.BreakStatement{Loc: stmt.Loc}
}

func (c *TypeChecker) checkContinueStatement(stmt *ast.ContinueStatement) *ast.ContinueStatement {
	if c.nestedLoops == 0 {
		c.errorf("%s: continue cannot be used outside a loop", stmt.Loc)
	}
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
	objectExpr := c.checkValueExpression(fa.Object)
	fieldType := c.getFieldType(fa.Loc, objectExpr, fa.FieldName)
	result := *fa
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
		c.errorf("%s: type %s used in field access is not a base type", loc, objectType)
		return ast.Int // Use a default type to avoid nil
	}

	structDesc, err := c.types.GetStruct(structType)
	if err != nil {
		c.errorf("%s: %v", loc, err)
		return ast.Int // Use a default type to avoid nil
	}

	if structDesc == nil {
		c.errorf("%s: cannot access field %s of a non-struct", loc, fieldName)
		return ast.Int // Use a default type to avoid nil
	}
	fieldType := structDesc.GetFieldType(fieldName)
	if fieldType == nil {
		c.errorf("%s: struct %s does not have field %s", loc, structDesc.Name, fieldName)
		return ast.Int // Use a default type to avoid nil
	}

	return fieldType
}

func (c *TypeChecker) checkNewExpression(n *ast.NewExpression) *ast.NewExpression {
	c.validateType(n.Loc, n.TypeExpr)
	if _, ok := n.TypeExpr.(*ast.SliceType); ok {
		if n.Count == nil {
			c.errorf("%s: when allocating a slice via new(), an element count must be specified", n.Loc)
			result := *n
			result.Type = ast.Undefined
			return &result
		}
		checkedCount := c.checkValueExpression(n.Count)
		// The backend only supports int-sized counts, same as resize().
		if countType := checkedCount.GetType(); !countType.Equals(ast.Int) {
			c.errorf("%s: element count in new() must have type %s, got %s", n.Loc, ast.Int, countType)
		}
		result := *n
		result.Type = n.TypeExpr
		result.Count = checkedCount
		return &result
	} else {
		if n.Count != nil {
			c.errorf("%s: an element count can only be specified when allocating a slice via new()", n.Loc)
		}
		result := *n
		result.Type = &ast.PointerType{ElementType: n.TypeExpr}
		return &result
	}
}

func (c *TypeChecker) checkPostfixOperator(po *ast.PostfixOperator) *ast.PostfixOperator {
	// TODO: Make postfix operators valid only in the expression statement context.
	var operandType ast.Type
	checkedOperand := c.checkValueExpression(po.Operand)
	// The only postfix operators currently supported (++ and --) work on integers.
	if ast.IsIntegerType(checkedOperand.GetType()) {
		operandType = checkedOperand.GetType()
	} else {
		operandType = ast.Undefined
		c.errorf("%s: postfix %s can only be applied to an integer type", po.Loc, po.Operator)
	}
	c.checkIncDecOperand(po.Loc, po.Operand, checkedOperand, po.Operator)
	result := *po
	result.Operand = checkedOperand
	result.Type = operandType
	return &result
}

func (c *TypeChecker) checkPrefixOperator(po *ast.PrefixOperator) *ast.PrefixOperator {
	var operandType ast.Type
	checkedOperand := c.checkValueExpression(po.Operand)
	// The only prefix operators currently supported (++ and --) work on integers.
	if ast.IsIntegerType(checkedOperand.GetType()) {
		operandType = checkedOperand.GetType()
	} else {
		operandType = ast.Undefined
		c.errorf("%s: prefix %s can only be applied to an integer type", po.Loc, po.Operator)
	}
	c.checkIncDecOperand(po.Loc, po.Operand, checkedOperand, po.Operator)
	result := *po
	result.Operand = checkedOperand
	result.Type = operandType
	return &result
}

// checkIncDecOperand checks that the operand of ++/-- is a modifiable location.
// It needs both the original operand (for looking up variables by their source name)
// and the checked one (for type information).
func (c *TypeChecker) checkIncDecOperand(loc ast.Location, operand, checkedOperand ast.Expression, operator string) {
	if name, isConst := c.constantRoot(operand, checkedOperand); isConst {
		c.errorf("%s: cannot apply %s to constant %s", loc, operator, name)
	}
	if !isAddressable(checkedOperand) {
		c.errorf("%s: invalid operand for %s", loc, operator)
	}
}

func (c *TypeChecker) checkIndexExpression(indexExpr *ast.IndexExpression) *ast.IndexExpression {
	arrayExpr := c.checkValueExpression(indexExpr.Array)
	indexExprChecked := c.checkValueExpression(indexExpr.Index)

	arrayType := arrayExpr.GetType()
	indexType := indexExprChecked.GetType()

	// Check that index is an integer type
	if !ast.IsIntegerType(indexType) {
		c.errorf("%s: slice index must be an integer type, got %s", indexExpr.Loc, indexType)
	}

	var elementType ast.Type
	if sliceType, ok := arrayType.(*ast.SliceType); ok {
		elementType = sliceType.ElementType
	} else if arrayType == ast.String {
		elementType = ast.Int8
	} else {
		c.errorf("%s: indexing requires a slice or string type, got %s", indexExpr.Loc, arrayType)
		// Return with error type to continue type checking
		result := *indexExpr
		result.Array = arrayExpr
		result.Index = indexExprChecked
		result.Type = ast.Undefined
		return &result
	}

	result := *indexExpr
	result.Array = arrayExpr
	result.Index = indexExprChecked
	result.Type = elementType
	return &result
}

func (c *TypeChecker) checkRangeExpression(rangeExpr *ast.RangeExpression) *ast.RangeExpression {
	arrayExpr := c.checkValueExpression(rangeExpr.Array)
	startExpr := c.checkValueExpression(rangeExpr.Start)
	endExpr := c.checkValueExpression(rangeExpr.End)

	arrayType := arrayExpr.GetType()
	startType := startExpr.GetType()
	endType := endExpr.GetType()

	// Check that start and end are integer types
	if !ast.IsIntegerType(startType) {
		c.errorf("%s: range start must be an integer type, got %s", rangeExpr.Loc, startType)
	}
	if !ast.IsIntegerType(endType) {
		c.errorf("%s: range end must be an integer type, got %s", rangeExpr.Loc, endType)
	}

	// Check that array is a slice type or string
	var resultType ast.Type
	if sliceType, ok := arrayType.(*ast.SliceType); ok {
		resultType = sliceType
	} else if arrayType == ast.String {
		resultType = ast.String
	} else {
		c.errorf("%s: range requires a slice or string type, got %s", rangeExpr.Loc, arrayType)
		resultType = ast.Undefined
	}

	result := *rangeExpr
	result.Array = arrayExpr
	result.Start = startExpr
	result.End = endExpr
	result.Type = resultType
	return &result
}

func (c *TypeChecker) unaryOperationResult(op string, val ast.Type) (ast.Type, bool) {
	switch op {
	case "!":
		return ast.Bool, val == ast.Bool
	case "-":
		return val, ast.IsNumericType(val)
	case "&":
		// Any type can be pointed to; addressability of the operand expression
		// is checked in checkUnaryOperation.
		return &ast.PointerType{ElementType: val}, true
	case "*":
		if ptr, ok := val.(*ast.PointerType); ok {
			return ptr.ElementType, true
		} else {
			return nil, false
		}
	}
	panic(fmt.Sprintf("unknown unary operation %s", op))
}

// blockReturns checks whether a block is guaranteed to return on all code paths.
func blockReturns(block *ast.Block) bool {
	for _, stmt := range block.Statements {
		if statementReturns(stmt) {
			return true
		}
	}
	return false
}

func statementReturns(stmt ast.Statement) bool {
	switch s := stmt.(type) {
	case *ast.ReturnStatement:
		return true
	case *ast.IfStatement:
		if s.ElseBlock == nil {
			return false
		}
		return blockReturns(&s.ThenBlock) && blockReturns(s.ElseBlock)
	case *ast.BlockStatement:
		return blockReturns(&s.Block)
	default:
		return false
	}
}

func (c *TypeChecker) errorf(format string, args ...any) {
	c.errors = append(c.errors, fmt.Errorf(format, args...))
}

func (c *TypeChecker) binaryOperationResult(op string, left, right ast.Type) (ast.Type, bool) {
	if !areCompatibleTypes(left, right) {
		return nil, false
	}

	if op == "==" || op == "!=" {
		return ast.Bool, c.isComparableType(left) && c.isComparableType(right)
	}

	if op == "%" {
		// No fmod() semantics for now, so % works on integers only.
		return left, ast.IsIntegerType(left)
	}

	if op == "+" || op == "-" || op == "/" || op == "*" {
		return left, ast.IsNumericType(left)
	}

	if op == "<" || op == ">" || op == "<=" || op == ">=" {
		return ast.Bool, ast.IsNumericType(left)
	}

	if op == "&&" || op == "||" {
		return ast.Bool, left == ast.Bool
	}

	panic(fmt.Sprintf("unknown binary operation %s", op))
}

// isComparableType reports whether ==/!= are supported for the type.
// Scalars are compared by value, strings by content, and structs field by field.
// Slices (and structs containing them) are not comparable.
func (c *TypeChecker) isComparableType(typ ast.Type) bool {
	if ast.IsNumericType(typ) || typ == ast.Bool || typ == ast.File ||
		ast.IsPointerType(typ) || typ == ast.NullPtr || ast.String.Equals(typ) {
		return true
	}
	if sd, err := c.types.GetStruct(typ); err == nil {
		for _, field := range sd.Fields {
			if !c.isComparableType(field.Type) {
				return false
			}
		}
		return true
	}
	return false
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
