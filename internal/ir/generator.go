package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/types"
	"github.com/iley/pirx/internal/util"
)

type Generator struct {
	errors         []error
	nextTempIndex  int
	nextLabelIndex int
	breakLabel     string
	continueLabel  string
	funcs          map[string]types.FuncProto
	// Local variables: name -> size in bytes.
	vars               map[string]int
	types              *types.TypeTable
	isExternalFunction bool
}

func NewGenerator() *Generator {
	return &Generator{
		nextLabelIndex: 1,
		funcs:          make(map[string]types.FuncProto),
	}
}

func (g *Generator) Generate(node *ast.Program) (IrProgram, []error) {
	irp := IrProgram{}

	typeTable, err := types.MakeTypeTable(node.TypeDeclarations)
	if err != nil {
		return IrProgram{}, []error{err}
	}

	g.types = typeTable

	for _, funcProto := range types.GetFunctionTable(node) {
		g.funcs[funcProto.Name] = funcProto
	}

	for _, function := range node.Functions {
		// Don't generate IR for functions that don't have a definition in the current compilation unit.
		if function.Body == nil {
			continue
		}
		irFunc := g.generateFunction(function)
		irp.Functions = append(irp.Functions, irFunc)
	}
	return irp, nil
}

func (g *Generator) generateFunction(node ast.Function) IrFunction {
	irfunc := IrFunction{
		Name:     node.Name,
		Args:     []string{},
		ArgSizes: []int{},
		Ops:      []Op{},
	}

	g.nextTempIndex = 1
	g.vars = make(map[string]int)
	g.isExternalFunction = node.External

	for _, arg := range node.Args {
		g.vars[arg.Name] = g.types.GetSizeNoError(arg.Type)
		irfunc.Args = append(irfunc.Args, arg.Name)
		irfunc.ArgSizes = append(irfunc.ArgSizes, g.types.GetSizeNoError(arg.Type))
	}

	// It's safe to dereference body here because we check the function has a body before generateFunction() is called.
	irfunc.Ops = g.generateBlockOps(*node.Body)

	// Add implicit return in case the function doesn't end with a return
	// We assume that presence of return with the right type is checked upstream.
	var implicitReturn Op
	if node.External {
		implicitReturn = ExternalReturn{Value: nil}
	} else {
		implicitReturn = Return{Value: nil}
	}

	if len(irfunc.Ops) == 0 {
		// Empty function - add bare return.
		irfunc.Ops = append(irfunc.Ops, implicitReturn)
	} else {
		// Check if the last operation is a return.
		lastOp := irfunc.Ops[len(irfunc.Ops)-1]
		_, isReturn := lastOp.(Return)
		_, isExternalReturn := lastOp.(ExternalReturn)
		if !isReturn && !isExternalReturn {
			// Last operation is not a return - add implicit bare return.
			irfunc.Ops = append(irfunc.Ops, implicitReturn)
		}
	}

	return irfunc
}

func (g *Generator) generateBlockOps(block ast.Block) []Op {
	ops := []Op{}
	for _, stmt := range block.Statements {
		stmtOps := g.generateStatementOps(stmt)
		ops = append(ops, stmtOps...)
	}
	return ops
}

func (g *Generator) generateStatementOps(node ast.Statement) []Op {
	ops := []Op{}
	if varDecl, ok := node.(*ast.VariableDeclaration); ok {
		initArg := Arg{Zero: true}
		if varDecl.Initializer != nil {
			var initOps []Op
			initOps, initArg, _ = g.generateExpressionOps(varDecl.Initializer)
			ops = append(ops, initOps...)
		}
		size := g.types.GetSizeNoError(varDecl.Type)
		g.vars[varDecl.Name] = size
		ops = append(ops, Assign{Size: size, Target: varDecl.Name, Value: initArg})
	} else if exprStmt, ok := node.(*ast.ExpressionStatement); ok {
		// We ignore the result of the expression.
		exprOps, _, _ := g.generateExpressionOps(exprStmt.Expression)
		ops = append(ops, exprOps...)
	} else if retStmt, ok := node.(*ast.ReturnStatement); ok {
		if retStmt.Value != nil {
			// Return with value
			exprOps, resultArg, resultSize := g.generateExpressionOps(retStmt.Value)
			ops = append(ops, exprOps...)
			// TODO: Maybe factor this out into a function that creates the right type of return?
			if g.isExternalFunction {
				ops = append(ops, ExternalReturn{Size: resultSize, Value: &resultArg})
			} else {
				ops = append(ops, Return{Size: resultSize, Value: &resultArg})
			}
		} else {
			// Bare return
			if g.isExternalFunction {
				ops = append(ops, ExternalReturn{Value: nil})
			} else {
				ops = append(ops, Return{Value: nil})
			}
		}
	} else if ifStmt, ok := node.(*ast.IfStatement); ok {
		ops = g.generateIfOps(*ifStmt)
	} else if stmt, ok := node.(*ast.WhileStatement); ok {
		ops = g.generateWhileOps(*stmt)
	} else if _, ok := node.(*ast.BreakStatement); ok {
		ops = append(ops, Jump{Goto: g.breakLabel})
	} else if _, ok := node.(*ast.ContinueStatement); ok {
		ops = append(ops, Jump{Goto: g.continueLabel})
	} else if stmt, ok := node.(*ast.BlockStatement); ok {
		blockOps := g.generateBlockOps(stmt.Block)
		ops = append(ops, blockOps...)
	} else {
		panic(fmt.Errorf("unknown statement type %v", node))
	}
	return ops
}

// generateExpressionOps generates a seequence of ops for a given expression.
// Returns a slice of ops, Arg representing the value (typically an intermediary), and the size of the value in bytes.
func (g *Generator) generateExpressionOps(node ast.Expression) ([]Op, Arg, int) {
	if literal, ok := node.(*ast.Literal); ok {
		return g.generateLiteralOps(literal)
	} else if assignment, ok := node.(*ast.Assignment); ok {
		return g.generateAssignmentOps(assignment)
	} else if call, ok := node.(*ast.FunctionCall); ok {
		return g.generateFunctionCallOps(call)
	} else if ref, ok := node.(*ast.VariableReference); ok {
		return []Op{}, Arg{Variable: ref.Name}, g.vars[ref.Name]
	} else if binOp, ok := node.(*ast.BinaryOperation); ok {
		return g.generateBinaryOperationOps(binOp)
	} else if op, ok := node.(*ast.UnaryOperation); ok {
		return g.generateUnaryOperationOps(op)
	} else if fa, ok := node.(*ast.FieldAccess); ok {
		return g.generateFieldAccessOps(fa)
	} else if ne, ok := node.(*ast.NewExpression); ok {
		return g.generateNewExpressionOps(ne)
	} else if index, ok := node.(*ast.IndexExpression); ok {
		return g.generateIndexOps(index)
	} else if po, ok := node.(*ast.PostfixOperator); ok {
		return g.generatePostfixOperatorOps(po)
	}
	panic(fmt.Errorf("unknown expression type: %v", node))
}

func (g *Generator) generateBinaryOperationOps(binOp *ast.BinaryOperation) ([]Op, Arg, int) {
	leftOps, leftArg, leftSize := g.generateExpressionOps(binOp.Left)
	rightOps, rightArg, rightSize := g.generateExpressionOps(binOp.Right)
	if leftSize != rightSize {
		g.errorf("%s: size mismatch between operands of %s", binOp.Loc, binOp.Operator)
		return []Op{}, Arg{}, 0
	}
	ops := append(leftOps, rightOps...)
	temp := g.allocTemp(leftSize)
	resultSize := binaryOperationSize(binOp.Operator, leftSize)
	ops = append(ops, BinaryOp{
		Result:      temp,
		Left:        leftArg,
		Right:       rightArg,
		Operation:   binOp.Operator,
		Size:        resultSize,
		OperandSize: leftSize,
	})
	return ops, Arg{Variable: temp}, resultSize
}

func (g *Generator) generateUnaryOperationOps(op *ast.UnaryOperation) ([]Op, Arg, int) {
	ops, arg, operandSize := g.generateExpressionOps(op.Operand)
	var resultSize int
	switch op.Operator {
	case "&":
		resultSize = types.WORD_SIZE
	case "*":
		pointerType, ok := op.Operand.GetType().(*ast.PointerType)
		if !ok {
			g.errorf("%s: expected a pointer type in dereference, got %v", op.Loc, op.Operand.GetType())
			return []Op{}, Arg{}, 0
		}
		var err error
		resultSize, err = g.types.GetSize(pointerType.ElementType)
		if err != nil {
			g.errorf("%s: unknown type size in dereference: %s", op.Loc, err)
			return []Op{}, Arg{}, 0
		}
	default:
		resultSize = operandSize
	}
	temp := g.allocTemp(resultSize)
	ops = append(ops, UnaryOp{Result: temp, Value: arg, Operation: op.Operator, Size: resultSize})
	return ops, Arg{Variable: temp}, resultSize
}

func (g *Generator) generateNewExpressionOps(ne *ast.NewExpression) ([]Op, Arg, int) {
	if sliceType, ok := ne.Type.(*ast.SliceType); ok {
		ops, sizeArg, sizeSize := g.generateExpressionOps(ne.Count)
		if sizeSize != types.INT_SIZE {
			// TODO: Proper error handling!
			g.errorf("%s: expected the size expression to have size %d, got %d", ne.Loc, types.INT_SIZE, sizeSize)
			return []Op{}, Arg{}, 0
		}
		elementSize := g.types.GetSizeNoError(sliceType.ElementType)
		res := g.allocTemp(types.SLICE_SIZE)
		allocCall := ExternalCall{
			Result:   res,
			Function: "Pirx_Slice_Alloc",
			// TODO: Support independent capacity argument.
			Args:      []Arg{{LiteralInt: util.Int64Ptr(int64(elementSize))}, sizeArg, sizeArg},
			ArgSizes:  []int{types.INT_SIZE, types.INT_SIZE, types.INT_SIZE},
			NamedArgs: 3,
			Size:      types.SLICE_SIZE,
		}
		ops = append(ops, allocCall)
		return ops, Arg{Variable: res}, types.SLICE_SIZE
	} else {
		// Regular type allocation -> return a pointer.
		allocSize := g.types.GetSizeNoError(ne.TypeExpr)
		res := g.allocTemp(types.WORD_SIZE)
		// TODO: Maybe make a helper function for generating such function calls?
		allocCall := ExternalCall{
			Result:    res,
			Function:  "Pirx_Alloc",
			Args:      []Arg{{LiteralInt: util.Int64Ptr(int64(allocSize))}},
			ArgSizes:  []int{types.WORD_SIZE},
			NamedArgs: 1,
			Size:      types.WORD_SIZE,
		}
		return []Op{allocCall}, Arg{Variable: res}, types.WORD_SIZE
	}
}

func (g *Generator) generateLiteralOps(literal *ast.Literal) ([]Op, Arg, int) {
	if literal.IntValue != nil {
		intValue := int64(*literal.IntValue)
		return []Op{}, Arg{LiteralInt: &intValue}, 4
	} else if literal.Int8Value != nil {
		intValue := int64(*literal.Int8Value)
		return []Op{}, Arg{LiteralInt: &intValue}, 1
	} else if literal.Int64Value != nil {
		return []Op{}, Arg{LiteralInt: literal.Int64Value}, 8
	} else if literal.StringValue != nil {
		return []Op{}, Arg{LiteralString: literal.StringValue}, 8
	} else if literal.BoolValue != nil {
		// Translate booleans into 32-bit integers.
		var intValue int64
		if *literal.BoolValue {
			intValue = 1
		} else {
			intValue = 0
		}
		return []Op{}, Arg{LiteralInt: &intValue}, 4
	} else if literal.NullValue {
		value := int64(0)
		return []Op{}, Arg{LiteralInt: &value}, types.WORD_SIZE
	} else {
		panic(fmt.Sprintf("invalid literal type: %#v", literal))
	}
}

// generateExpressionAddrOps is similar to generateExpressionOps but it produces the address of the result rather than the value.
// The size is always WORD_SIZE.
func (g *Generator) generateExpressionAddrOps(node ast.Expression) ([]Op, Arg) {
	// Literals, function calls, assignments etc. are not supported.
	if ref, ok := node.(*ast.VariableReference); ok {
		res := g.allocTemp(types.WORD_SIZE)
		ops := []Op{UnaryOp{Result: res, Operation: "&", Value: Arg{Variable: ref.Name}, Size: types.WORD_SIZE}}
		return ops, Arg{Variable: res}
	} else if op, ok := node.(*ast.UnaryOperation); ok {
		if op.Operator == "*" {
			ops, arg, size := g.generateExpressionOps(op.Operand)
			if size != types.WORD_SIZE {
				g.errorf("%s: invalid expression size in dereference: %d, expected word size", node.GetLocation(), size)
			}
			return ops, arg
		}
		panic(fmt.Errorf("unsupported unary operation %s in generateExpressionAddrOps", op.Operator))
	} else if fa, ok := node.(*ast.FieldAccess); ok {
		return g.generateFieldAccessAddrOps(fa.Object, fa.FieldName)
	} else if index, ok := node.(*ast.IndexExpression); ok {
		return g.generateIndexAddrOps(index.Array, index.Index)
	}
	panic(fmt.Errorf("unsupported expression in generateExpressionAddrOps: %#v", node))
}

func (g *Generator) generateFieldAccessOps(fa *ast.FieldAccess) ([]Op, Arg, int) {
	// Get field address using generateExpressionAddrOps.
	ops, addrArg := g.generateExpressionAddrOps(fa)
	// Dereference to get the value.
	field := g.getField(fa.Loc, fa.Object.GetType(), fa.FieldName)
	res := g.allocTemp(field.Size)
	ops = append(ops, UnaryOp{Result: res, Value: addrArg, Operation: "*", Size: field.Size})
	return ops, Arg{Variable: res}, field.Size
}

func (g *Generator) generateFieldAccessAddrOps(object ast.Expression, fieldName string) ([]Op, Arg) {
	var ops []Op
	var objArg Arg

	if ast.IsPointerType(object.GetType()) {
		// If left side is a pointer to the struct, just evaluate it.
		ops, objArg, _ = g.generateExpressionOps(object)
	} else {
		// If struct value, get the address.
		ops, objArg = g.generateExpressionAddrOps(object)
	}

	// Add offset.
	addrTemp := g.allocTemp(types.WORD_SIZE)
	field := g.getField(object.GetLocation(), object.GetType(), fieldName)
	offset := int64(field.Offset)
	ops = append(ops, BinaryOp{
		Result:      addrTemp,
		Left:        objArg,
		Operation:   "+",
		Right:       Arg{LiteralInt: &offset},
		Size:        types.WORD_SIZE,
		OperandSize: types.WORD_SIZE,
	})
	// Dereference.
	return ops, Arg{Variable: addrTemp}
}

func (g *Generator) generateIfOps(stmt ast.IfStatement) []Op {
	ops := []Op{}

	if stmt.ElseBlock == nil {
		endLabel := g.allocLabel()
		condOps, condArg, condSize := g.generateExpressionOps(stmt.Condition)
		ops = append(ops, condOps...)
		// FIXME: For some reason condSize is 8 when it should be 4???
		ops = append(ops, JumpUnless{Condition: condArg, Goto: endLabel, Size: condSize})
		blockOps := g.generateBlockOps(stmt.ThenBlock)
		ops = append(ops, blockOps...)
		ops = append(ops, Anchor{Label: endLabel})
	} else {
		elseLabel := g.allocLabel()
		endLabel := g.allocLabel()
		condOps, condArg, condSize := g.generateExpressionOps(stmt.Condition)
		ops = append(ops, condOps...)
		ops = append(ops, JumpUnless{Condition: condArg, Goto: elseLabel, Size: condSize})
		thenOps := g.generateBlockOps(stmt.ThenBlock)
		ops = append(ops, thenOps...)
		ops = append(ops, Jump{Goto: endLabel})
		ops = append(ops, Anchor{Label: elseLabel})
		elseOps := g.generateBlockOps(*stmt.ElseBlock)
		ops = append(ops, elseOps...)
		ops = append(ops, Anchor{Label: endLabel})
	}

	return ops
}

func (g *Generator) generateWhileOps(stmt ast.WhileStatement) []Op {
	prevBreakLabel := g.breakLabel
	prevContinueLabel := g.continueLabel

	ops := []Op{}
	g.continueLabel = g.allocLabel()
	g.breakLabel = g.allocLabel()

	ops = append(ops, Anchor{Label: g.continueLabel})
	condOps, condArg, condSize := g.generateExpressionOps(stmt.Condition)
	ops = append(ops, condOps...)
	ops = append(ops, JumpUnless{Condition: condArg, Goto: g.breakLabel, Size: condSize})
	blockOps := g.generateBlockOps(stmt.Body)
	ops = append(ops, blockOps...)
	ops = append(ops, Jump{Goto: g.continueLabel})
	ops = append(ops, Anchor{Label: g.breakLabel})

	g.breakLabel = prevBreakLabel
	g.continueLabel = prevContinueLabel

	return ops
}

func (g *Generator) generateAssignmentOps(assgn *ast.Assignment) ([]Op, Arg, int) {
	if targetVar, ok := assgn.Target.(*ast.VariableReference); ok {
		ops, rvalueArg, rvalueSize := g.generateExpressionOps(assgn.Value)
		ops = append(ops, Assign{Target: targetVar.Name, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	} else if targetRef, ok := assgn.Target.(*ast.UnaryOperation); ok && targetRef.Operator == "*" {
		ops, rvalueArg, rvalueSize := g.generateExpressionOps(assgn.Value)
		lvalueOps, lvalueArg, lvalueSize := g.generateExpressionOps(targetRef.Operand)
		ops = append(ops, lvalueOps...)
		addrTemp := g.allocTemp(lvalueSize)
		ops = append(ops, Assign{Target: addrTemp, Value: lvalueArg, Size: lvalueSize})
		ops = append(ops, AssignByAddr{Target: lvalueArg, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	} else if fieldAccess, ok := assgn.Target.(*ast.FieldAccess); ok {
		rvalueOps, rvalueArg, rvalueSize := g.generateExpressionOps(assgn.Value)
		fieldAddrOps, fieldAddrArg := g.generateFieldAccessAddrOps(fieldAccess.Object, fieldAccess.FieldName)
		ops := append(rvalueOps, fieldAddrOps...)
		ops = append(ops, AssignByAddr{Target: fieldAddrArg, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	} else if index, ok := assgn.Target.(*ast.IndexExpression); ok {
		rvalueOps, rvalueArg, rvalueSize := g.generateExpressionOps(assgn.Value)
		indexAddrOps, indexAddrArg := g.generateIndexAddrOps(index.Array, index.Index)
		ops := append(rvalueOps, indexAddrOps...)
		ops = append(ops, AssignByAddr{Target: indexAddrArg, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	}
	panic(fmt.Errorf("invalid assignment: %#v", assgn))
}

// generateExternalCall creates an ExternalCall operation with the appropriate function name.
func (g *Generator) generateExternalCall(funcProto types.FuncProto, resultVar string, args []Arg, sizes []int, size int) ExternalCall {
	name := funcProto.Name
	if funcProto.ExternalName != "" {
		name = funcProto.ExternalName
	}
	return ExternalCall{
		Result:    resultVar,
		Function:  name,
		Args:      args,
		ArgSizes:  sizes,
		NamedArgs: len(funcProto.Args),
		Size:      size,
	}
}

func (g *Generator) generateDisposeCall(call *ast.FunctionCall, resultVar string, arg Arg) ExternalCall {
	if ast.IsPointerType(call.Args[0].GetType()) {
		return ExternalCall{
			Result:    resultVar,
			Function:  "Pirx_Dispose",
			Args:      []Arg{arg},
			ArgSizes:  []int{types.WORD_SIZE},
			NamedArgs: 1,
			Size:      types.WORD_SIZE,
		}
	} else if ast.IsSliceType(call.Args[0].GetType()) {
		return ExternalCall{
			Result:    resultVar,
			Function:  "Pirx_Slice_Dispose",
			Args:      []Arg{arg},
			ArgSizes:  []int{types.SLICE_SIZE},
			NamedArgs: 1,
			Size:      types.WORD_SIZE,
		}
	} else {
		panic(fmt.Errorf("%s: invalid argument type for dispose(): %s", call.Loc, call.Args[0].GetType()))
	}
}

// generateFunctionCallOps generates ops for a function.
// Returns a slice of ops, an Arg representing the return value, and the size of the return type in bytes.
func (g *Generator) generateFunctionCallOps(call *ast.FunctionCall) ([]Op, Arg, int) {
	ops := []Op{}
	args := []Arg{}
	sizes := []int{}
	for _, argNode := range call.Args {
		// TODO: What to do with the size of the argument?
		subOps, subArg, subSize := g.generateExpressionOps(argNode)
		ops = append(ops, subOps...)
		args = append(args, subArg)
		sizes = append(sizes, subSize)
	}

	funcProto, ok := g.funcs[call.FunctionName]
	if !ok {
		// We assume this is handled by typecheck.
		panic(fmt.Sprintf("unknown function %s", call.FunctionName))
	}

	if !funcProto.Variadic && len(args) != len(funcProto.Args) {
		// We assume this is handled by typecheck.
		panic(fmt.Sprintf("argument mismatch for function %s: expected %d arguments, got %d", call.FunctionName, len(args), len(funcProto.Args)))
	}

	// This is a workaround for void functions. For now we just make it look like they return a word.
	// TODO: Handle void functions better. Omit the assignment. Perhaps introduce a null target.
	size := types.WORD_SIZE
	if funcProto.ReturnType != nil {
		size = g.types.GetSizeNoError(funcProto.ReturnType)
	}

	temp := g.allocTemp(size)
	if funcProto.External {
		var externalCall ExternalCall
		if funcProto.Name == "dispose" {
			externalCall = g.generateDisposeCall(call, temp, args[0])
		} else {
			externalCall = g.generateExternalCall(funcProto, temp, args, sizes, size)
		}
		ops = append(ops, externalCall)
	} else {
		ops = append(ops, Call{
			Result:   temp,
			Function: call.FunctionName,
			Args:     args,
			ArgSizes: sizes,
			Size:     size,
		})
	}

	return ops, Arg{Variable: temp}, size
}

func (g *Generator) getField(loc ast.Location, objType ast.Type, fieldName string) *types.StructField {
	var structType ast.Type

	if ptrType, ok := objType.(*ast.PointerType); ok {
		structType = ptrType.ElementType
	} else {
		structType = objType
	}

	structDesc, err := g.types.GetStruct(structType)
	if err != nil {
		panic(fmt.Errorf("%s: field access for non-struct types is not (yet) supported. type %v, error: %v", loc, structType, err))
	}
	field := structDesc.GetField(fieldName)
	if field == nil {
		g.errorf("%s: struct type %v has no field %v", loc, structDesc.Name, fieldName)
	}
	return field
}

func (g *Generator) generateIndexAddrOps(sliceExpr, indexExpr ast.Expression) ([]Op, Arg) {
	// TODO: Generate boundary check!
	sliceOps, sliceArg, _ := g.generateExpressionOps(sliceExpr)
	indexOps, indexArg, indexSize := g.generateExpressionOps(indexExpr)
	if indexSize != types.INT_SIZE {
		panic(fmt.Errorf("%s: expected size of the index value to be %d, got %d", sliceExpr.GetLocation(), types.INT_SIZE, indexSize))
	}

	sliceType, ok := sliceExpr.GetType().(*ast.SliceType)
	if !ok {
		panic(fmt.Errorf("%s: cannot index an element of non-slice type %s", sliceExpr.GetLocation(), sliceExpr.GetType()))
	}
	elementSize := int64(g.types.GetSizeNoError(sliceType.ElementType))

	ops := append(sliceOps, indexOps...)

	addrTemp := g.allocTemp(types.WORD_SIZE)
	ops = append(
		ops,
		// Not the implicit 4->8 extension.
		BinaryOp{
			Result:      addrTemp,
			Left:        indexArg,
			Operation:   "*",
			Right:       Arg{LiteralInt: &elementSize},
			OperandSize: types.INT_SIZE,
			Size:        types.WORD_SIZE,
		},
		// This is a bit of a hack: we're treating the slice as if it was a single 64-bit address field.
		// This gives us the access to the .data field which is the first field in the slice struct.
		BinaryOp{
			Result:      addrTemp,
			Left:        Arg{Variable: addrTemp},
			Operation:   "+",
			Right:       sliceArg,
			OperandSize: types.WORD_SIZE,
			Size:        types.WORD_SIZE,
		},
	)

	return ops, Arg{Variable: addrTemp}
}

func (g *Generator) generateIndexOps(indexExpr *ast.IndexExpression) ([]Op, Arg, int) {
	// TODO: Generate boundary check!
	elementSize := g.types.GetSizeNoError(indexExpr.GetType())
	addrOps, addrArg := g.generateIndexAddrOps(indexExpr.Array, indexExpr.Index)
	res := g.allocTemp(elementSize)
	ops := append(addrOps, UnaryOp{Result: res, Value: addrArg, Operation: "*", Size: elementSize})
	return ops, Arg{Variable: res}, elementSize
}

func (g *Generator) generatePostfixOperatorOps(expr *ast.PostfixOperator) ([]Op, Arg, int) {
	// TODO: Generate more optimial IR. Perhaps make the increment an unary op?
	var operator string
	switch expr.Operator {
	case "++":
		operator = "+"
	case "--":
		operator = "-"
	default:
		panic(fmt.Errorf("%s: unsupported postfix operator %s", expr.Loc, expr.Operator))
	}
	operandSize := g.types.GetSizeNoError(expr.GetType())
	ops, operandArg := g.generateExpressionAddrOps(expr.Operand)
	temp := g.allocTemp(operandSize)
	ops = append(ops,
		UnaryOp{
			Result:    temp,
			Operation: "*",
			Value:     operandArg,
			Size:      operandSize,
		},
		BinaryOp{
			Result:      temp,
			Left:        Arg{Variable: temp},
			Operation:   operator,
			Right:       Arg{LiteralInt: util.Int64Ptr(1)},
			Size:        operandSize,
			OperandSize: operandSize,
		},
		AssignByAddr{
			Target: operandArg,
			Value:  Arg{Variable: temp},
			Size:   operandSize,
		},
	)
	return ops, Arg{Variable: temp}, operandSize
}

func (g *Generator) errorf(format string, arg ...any) {
	g.errors = append(g.errors, fmt.Errorf(format, arg...))
}

func (g *Generator) allocTemp(size int) string {
	idx := g.nextTempIndex
	g.nextTempIndex++
	name := fmt.Sprintf("$%d", idx)
	g.vars[name] = size
	return name
}

func (g *Generator) allocLabel() string {
	idx := g.nextLabelIndex
	g.nextLabelIndex++
	return fmt.Sprintf("anchor%d", idx)
}

func binaryOperationSize(operator string, operandSize int) int {
	switch operator {
	case "==", "!=", "<", ">", "<=", ">=":
		return types.BOOL_SIZE
	}
	return operandSize
}
