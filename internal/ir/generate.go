package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/types"
	"github.com/iley/pirx/internal/util"
)

type IrContext struct {
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

func (ic *IrContext) allocTemp(size int) string {
	idx := ic.nextTempIndex
	ic.nextTempIndex++
	name := fmt.Sprintf("$%d", idx)
	ic.vars[name] = size
	return name
}

func (ic *IrContext) allocLabel() string {
	idx := ic.nextLabelIndex
	ic.nextLabelIndex++
	return fmt.Sprintf("anchor%d", idx)
}

func Generate(node *ast.Program) (IrProgram, error) {
	irp := IrProgram{}
	ic := IrContext{
		nextLabelIndex: 1,
		funcs:          make(map[string]types.FuncProto),
	}

	typeTable, err := types.MakeTypeTable(node.TypeDeclarations)
	if err != nil {
		return IrProgram{}, err
	}

	ic.types = typeTable

	for _, funcProto := range types.GetFunctionTable(node) {
		ic.funcs[funcProto.Name] = funcProto
	}

	for _, function := range node.Functions {
		// Don't generate IR for functions that don't have a definition in the current compilation unit.
		if function.Body == nil {
			continue
		}
		irFunc := generateFunction(&ic, function)
		irp.Functions = append(irp.Functions, irFunc)
	}
	return irp, nil
}

func generateFunction(ic *IrContext, node ast.Function) IrFunction {
	irfunc := IrFunction{
		Name:     node.Name,
		Args:     []string{},
		ArgSizes: []int{},
		Ops:      []Op{},
	}

	// Create a new context for this function
	fic := *ic
	fic.nextTempIndex = 1
	fic.vars = make(map[string]int)
	fic.isExternalFunction = node.External

	for _, arg := range node.Args {
		fic.vars[arg.Name] = ic.types.GetSizeNoError(arg.Type)
		irfunc.Args = append(irfunc.Args, arg.Name)
		irfunc.ArgSizes = append(irfunc.ArgSizes, ic.types.GetSizeNoError(arg.Type))
	}

	// It's safe to dereference body here because we check the function has a body before generateFunction() is called.
	irfunc.Ops = generateBlockOps(&fic, *node.Body)

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

func generateBlockOps(ic *IrContext, block ast.Block) []Op {
	ops := []Op{}
	for _, stmt := range block.Statements {
		stmtOps := generateStatementOps(ic, stmt)
		ops = append(ops, stmtOps...)
	}
	return ops
}

func generateStatementOps(ic *IrContext, node ast.Statement) []Op {
	ops := []Op{}
	if varDecl, ok := node.(*ast.VariableDeclaration); ok {
		initArg := Arg{Zero: true}
		if varDecl.Initializer != nil {
			var initOps []Op
			initOps, initArg, _ = generateExpressionOps(ic, varDecl.Initializer)
			ops = append(ops, initOps...)
		}
		size := ic.types.GetSizeNoError(varDecl.Type)
		ic.vars[varDecl.Name] = size
		ops = append(ops, Assign{Size: size, Target: varDecl.Name, Value: initArg})
	} else if exprStmt, ok := node.(*ast.ExpressionStatement); ok {
		// We ignore the result of the expression.
		exprOps, _, _ := generateExpressionOps(ic, exprStmt.Expression)
		ops = append(ops, exprOps...)
	} else if retStmt, ok := node.(*ast.ReturnStatement); ok {
		if retStmt.Value != nil {
			// Return with value
			exprOps, resultArg, resultSize := generateExpressionOps(ic, retStmt.Value)
			ops = append(ops, exprOps...)
			// TODO: Maybe factor this out into a function that creates the right type of return?
			if ic.isExternalFunction {
				ops = append(ops, ExternalReturn{Size: resultSize, Value: &resultArg})
			} else {
				ops = append(ops, Return{Size: resultSize, Value: &resultArg})
			}
		} else {
			// Bare return
			if ic.isExternalFunction {
				ops = append(ops, ExternalReturn{Value: nil})
			} else {
				ops = append(ops, Return{Value: nil})
			}
		}
	} else if ifStmt, ok := node.(*ast.IfStatement); ok {
		ops = generateIfOps(ic, *ifStmt)
	} else if stmt, ok := node.(*ast.WhileStatement); ok {
		ops = generateWhileOps(ic, *stmt)
	} else if _, ok := node.(*ast.BreakStatement); ok {
		ops = append(ops, Jump{Goto: ic.breakLabel})
	} else if _, ok := node.(*ast.ContinueStatement); ok {
		ops = append(ops, Jump{Goto: ic.continueLabel})
	} else if stmt, ok := node.(*ast.BlockStatement); ok {
		blockOps := generateBlockOps(ic, stmt.Block)
		ops = append(ops, blockOps...)
	} else {
		panic(fmt.Sprintf("unknown statement type %v", node))
	}
	return ops
}

// generateExpressionOps generates a seequence of ops for a given expression.
// Returns a slice of ops, Arg representing the value (typically an intermediary), and the size of the value in bytes.
func generateExpressionOps(ic *IrContext, node ast.Expression) ([]Op, Arg, int) {
	if literal, ok := node.(*ast.Literal); ok {
		return generateLiteralOps(ic, literal)
	} else if assignment, ok := node.(*ast.Assignment); ok {
		return generateAssignmentOps(ic, assignment)
	} else if call, ok := node.(*ast.FunctionCall); ok {
		return generateFunctionCallOps(ic, call)
	} else if ref, ok := node.(*ast.VariableReference); ok {
		return []Op{}, Arg{Variable: ref.Name}, ic.vars[ref.Name]
	} else if binOp, ok := node.(*ast.BinaryOperation); ok {
		return generateBinaryOperationOps(ic, binOp)
	} else if op, ok := node.(*ast.UnaryOperation); ok {
		return generateUnaryOperationOps(ic, op)
	} else if fa, ok := node.(*ast.FieldAccess); ok {
		return generateFieldAccessOps(ic, fa)
	} else if ne, ok := node.(*ast.NewExpression); ok {
		return generateNewExpressionOps(ic, ne)
	}
	panic(fmt.Sprintf("Unknown expression type: %v", node))
}

func generateBinaryOperationOps(ic *IrContext, binOp *ast.BinaryOperation) ([]Op, Arg, int) {
	leftOps, leftArg, leftSize := generateExpressionOps(ic, binOp.Left)
	rightOps, rightArg, rightSize := generateExpressionOps(ic, binOp.Right)
	if leftSize != rightSize {
		panic(fmt.Sprintf("%s: size mismatch between operands of %s", binOp.Loc, binOp.Operator))
	}
	ops := append(leftOps, rightOps...)
	temp := ic.allocTemp(leftSize)
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

func generateUnaryOperationOps(ic *IrContext, op *ast.UnaryOperation) ([]Op, Arg, int) {
	ops, arg, operandSize := generateExpressionOps(ic, op.Operand)
	var resultSize int
	switch op.Operator {
	case "&":
		resultSize = types.WORD_SIZE
	case "*":
		pointerType, ok := op.Operand.GetType().(*ast.PointerType)
		if !ok {
			panic(fmt.Errorf("expected a pointer type in dereference, got %v", op.Operand.GetType()))
		}
		var err error
		resultSize, err = ic.types.GetSize(pointerType.ElementType)
		if err != nil {
			panic(fmt.Errorf("cannot find type size in dereference: %v", err))
		}
	default:
		resultSize = operandSize
	}
	temp := ic.allocTemp(resultSize)
	ops = append(ops, UnaryOp{Result: temp, Value: arg, Operation: op.Operator, Size: resultSize})
	return ops, Arg{Variable: temp}, resultSize
}

func generateNewExpressionOps(ic *IrContext, ne *ast.NewExpression) ([]Op, Arg, int) {
	allocSize, err := ic.types.GetSize(ne.TypeExpr)
	if err != nil {
		panic(err)
	}
	res := ic.allocTemp(types.WORD_SIZE)
	// TODO: Maybe make a helper function for generating such function calls?
	mallocCall := ExternalCall{
		Result:    res,
		Function:  "malloc",
		Args:      []Arg{{LiteralInt: util.Int64Ptr(int64(allocSize))}},
		ArgSizes:  []int{types.WORD_SIZE},
		NamedArgs: 1,
		Size:      types.WORD_SIZE,
	}
	return []Op{mallocCall}, Arg{Variable: res}, types.WORD_SIZE
}

func generateLiteralOps(_ *IrContext, literal *ast.Literal) ([]Op, Arg, int) {
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
		if *literal.BoolValue == true {
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
func generateExpressionAddrOps(ic *IrContext, node ast.Expression) ([]Op, Arg) {
	// Literals, function calls, assignments etc. are not supported.
	if ref, ok := node.(*ast.VariableReference); ok {
		res := ic.allocTemp(types.WORD_SIZE)
		ops := []Op{UnaryOp{Result: res, Operation: "&", Value: Arg{Variable: ref.Name}, Size: types.WORD_SIZE}}
		return ops, Arg{Variable: res}
	} else if op, ok := node.(*ast.UnaryOperation); ok {
		if op.Operator == "*" {
			ops, arg, size := generateExpressionOps(ic, op.Operand)
			if size != types.WORD_SIZE {
				panic(fmt.Errorf("invalid expression size in dereference: %d, expected word size", size))
			}
			return ops, arg
		}
		panic(fmt.Errorf("unsupported unary operation %s in generateExpressionAddrOps", op.Operator))
	} else if fa, ok := node.(*ast.FieldLValue); ok {
		return generateFieldAccessAddrOps(ic, fa.Object, fa.FieldName)
	} else if fa, ok := node.(*ast.FieldAccess); ok {
		return generateFieldAccessAddrOps(ic, fa.Object, fa.FieldName)
	}
	panic(fmt.Errorf("unsupported expression in generateExpressionAddrOps: %#v", node))
}

func generateFieldAccessOps(ic *IrContext, fa *ast.FieldAccess) ([]Op, Arg, int) {
	// Get field address using generateExpressionAddrOps.
	ops, addrArg := generateExpressionAddrOps(ic, fa)
	// Dereference to get the value.
	field := getField(ic, fa.Object.GetType(), fa.FieldName)
	res := ic.allocTemp(field.Size)
	ops = append(ops, UnaryOp{Result: res, Value: addrArg, Operation: "*", Size: field.Size})
	return ops, Arg{Variable: res}, field.Size
}

func generateFieldAccessAddrOps(ic *IrContext, object ast.Expression, fieldName string) ([]Op, Arg) {
	var ops []Op
	var objArg Arg

	if ast.IsPointerType(object.GetType()) {
		// If left side is a pointer to the struct, just evaluate it.
		ops, objArg, _ = generateExpressionOps(ic, object)
	} else {
		// If struct value, get the address.
		ops, objArg = generateExpressionAddrOps(ic, object)
	}

	// Add offset.
	addrTemp := ic.allocTemp(types.WORD_SIZE)
	field := getField(ic, object.GetType(), fieldName)
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

func generateIfOps(ic *IrContext, stmt ast.IfStatement) []Op {
	ops := []Op{}

	if stmt.ElseBlock == nil {
		endLabel := ic.allocLabel()
		condOps, condArg, condSize := generateExpressionOps(ic, stmt.Condition)
		ops = append(ops, condOps...)
		// FIXME: For some reason condSize is 8 when it should be 4???
		ops = append(ops, JumpUnless{Condition: condArg, Goto: endLabel, Size: condSize})
		blockOps := generateBlockOps(ic, stmt.ThenBlock)
		ops = append(ops, blockOps...)
		ops = append(ops, Anchor{Label: endLabel})
	} else {
		elseLabel := ic.allocLabel()
		endLabel := ic.allocLabel()
		condOps, condArg, condSize := generateExpressionOps(ic, stmt.Condition)
		ops = append(ops, condOps...)
		ops = append(ops, JumpUnless{Condition: condArg, Goto: elseLabel, Size: condSize})
		thenOps := generateBlockOps(ic, stmt.ThenBlock)
		ops = append(ops, thenOps...)
		ops = append(ops, Jump{Goto: endLabel})
		ops = append(ops, Anchor{Label: elseLabel})
		elseOps := generateBlockOps(ic, *stmt.ElseBlock)
		ops = append(ops, elseOps...)
		ops = append(ops, Anchor{Label: endLabel})
	}

	return ops
}

func generateWhileOps(ic *IrContext, stmt ast.WhileStatement) []Op {
	prevBreakLabel := ic.breakLabel
	prevContinueLabel := ic.continueLabel

	ops := []Op{}
	ic.continueLabel = ic.allocLabel()
	ic.breakLabel = ic.allocLabel()

	ops = append(ops, Anchor{Label: ic.continueLabel})
	condOps, condArg, condSize := generateExpressionOps(ic, stmt.Condition)
	ops = append(ops, condOps...)
	ops = append(ops, JumpUnless{Condition: condArg, Goto: ic.breakLabel, Size: condSize})
	blockOps := generateBlockOps(ic, stmt.Body)
	ops = append(ops, blockOps...)
	ops = append(ops, Jump{Goto: ic.continueLabel})
	ops = append(ops, Anchor{Label: ic.breakLabel})

	ic.breakLabel = prevBreakLabel
	ic.continueLabel = prevContinueLabel

	return ops
}

func generateAssignmentOps(ic *IrContext, assgn *ast.Assignment) ([]Op, Arg, int) {
	if targetVar, ok := assgn.Target.(*ast.VariableLValue); ok {
		ops, rvalueArg, rvalueSize := generateExpressionOps(ic, assgn.Value)
		ops = append(ops, Assign{Target: targetVar.Name, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	} else if targetRef, ok := assgn.Target.(*ast.DereferenceLValue); ok {
		ops, lvalueArg, lvalueSize := generateExpressionOps(ic, targetRef.Expression)
		rvalueOps, rvalueArg, rvalueSize := generateExpressionOps(ic, assgn.Value)
		ops = append(ops, rvalueOps...)
		addrTemp := ic.allocTemp(lvalueSize)
		ops = append(ops, Assign{Target: addrTemp, Value: lvalueArg, Size: lvalueSize})
		ops = append(ops, AssignByAddr{Target: lvalueArg, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	} else if fieldAccess, ok := assgn.Target.(*ast.FieldLValue); ok {
		// Get the field's address using the existing helper function.
		fieldAddrOps, fieldAddrArg := generateFieldAccessAddrOps(ic, fieldAccess.Object, fieldAccess.FieldName)

		// Calculate the rvalue we're assigning.
		rvalueOps, rvalueArg, rvalueSize := generateExpressionOps(ic, assgn.Value)

		// Combine operations and write to the field address.
		ops := append(fieldAddrOps, rvalueOps...)
		ops = append(ops, AssignByAddr{Target: fieldAddrArg, Value: rvalueArg, Size: rvalueSize})

		return ops, rvalueArg, rvalueSize
	}
	panic(fmt.Errorf("invalid assignment: %#v", assgn))
}

// generateFunctionCallOps generates ops for a function.
// Returns a slice of ops, an Arg representing the return value, and the size of the return type in bytes.
func generateFunctionCallOps(ic *IrContext, call *ast.FunctionCall) ([]Op, Arg, int) {
	ops := []Op{}
	args := []Arg{}
	sizes := []int{}
	for _, argNode := range call.Args {
		// TODO: What to do with the size of the argument?
		subOps, subArg, subSize := generateExpressionOps(ic, argNode)
		ops = append(ops, subOps...)
		args = append(args, subArg)
		sizes = append(sizes, subSize)
	}

	funcProto, ok := ic.funcs[call.FunctionName]
	if !ok {
		panic(fmt.Sprintf("unknown function %s", call.FunctionName))
	}

	if !funcProto.Variadic && len(args) != len(funcProto.Args) {
		panic(fmt.Sprintf("argument mismatch for function %s: expected %d arguments, got %d", call.FunctionName, len(args), len(funcProto.Args)))
	}

	// This is a workaround for void functions. For now we just make it look like they return a word.
	// TODO: Handle void functions better. Omit the assignment. Perhaps introduce a null target.
	size := types.WORD_SIZE
	if funcProto.ReturnType != nil {
		size = ic.types.GetSizeNoError(funcProto.ReturnType)
	}

	temp := ic.allocTemp(size)
	if funcProto.External {
		ops = append(ops, ExternalCall{
			Result:    temp,
			Function:  call.FunctionName,
			Args:      args,
			ArgSizes:  sizes,
			NamedArgs: len(funcProto.Args),
			Size:      size,
		})
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

func getField(ic *IrContext, objType ast.Type, fieldName string) *types.StructField {
	var structType ast.Type

	if ptrType, ok := objType.(*ast.PointerType); ok {
		structType = ptrType.ElementType
	} else {
		structType = objType
	}

	structDesc, err := ic.types.GetStruct(structType)
	if err != nil {
		panic(fmt.Errorf("field access for non-struct types is not (yet) supported. type %v, error: %v", objType, err))
	}
	field := structDesc.GetField(fieldName)
	if field == nil {
		panic(fmt.Errorf("struct type %v has no field %v", structDesc.Name, fieldName))
	}
	return field
}

func binaryOperationSize(operator string, operandSize int) int {
	switch operator {
	case "==", "!=", "<", ">", "<=", ">=":
		return types.BOOL_SIZE
	}
	return operandSize
}
