package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/functions"
	"github.com/iley/pirx/internal/types"
)

const (
	// TODO: Do we need to pass this in as a parameter?
	WORD_SIZE = 8
	BOOL_SIZE = 4
)

type IrContext struct {
	nextTempIndex  int
	nextLabelIndex int
	breakLabel     string
	continueLabel  string
	funcs          map[string]functions.Proto
	// Local variables: name -> size in bytes.
	vars map[string]int
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

func Generate(node *ast.Program) IrProgram {
	irp := IrProgram{}
	ic := IrContext{
		nextLabelIndex: 1,
		funcs:          make(map[string]functions.Proto),
	}

	for _, funcProto := range functions.GetFunctionTable(node) {
		ic.funcs[funcProto.Name] = funcProto
	}

	for _, function := range node.Functions {
		irFunc := generateFunction(&ic, function)
		irp.Functions = append(irp.Functions, irFunc)
	}
	return irp
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

	for _, arg := range node.Args {
		fic.vars[arg.Name] = getTypeSize(arg.Type)
		irfunc.Args = append(irfunc.Args, arg.Name)
		irfunc.ArgSizes = append(irfunc.ArgSizes, getTypeSize(arg.Type))
	}

	irfunc.Ops = generateBlockOps(&fic, node.Body)

	// Add implicit return if the function doesn't end with a return
	// We assume that presence of return with the right type is checked upstream.
	if len(irfunc.Ops) == 0 {
		// Empty function - add bare return
		irfunc.Ops = append(irfunc.Ops, Return{Value: nil})
	} else {
		// Check if the last operation is a return
		lastOp := irfunc.Ops[len(irfunc.Ops)-1]
		if _, isReturn := lastOp.(Return); !isReturn {
			// Last operation is not a return - add implicit bare return
			irfunc.Ops = append(irfunc.Ops, Return{Value: nil})
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
		size := getTypeSize(varDecl.Type)
		ic.vars[varDecl.Name] = size
		// TODO: Handle more type sizes.
		switch size {
		case 4: // 32-bit values.
			zero := int32(0)
			ops = append(ops, Assign{Size: size, Target: varDecl.Name, Value: Arg{LiteralInt: &zero}})
		case 8: // 64-bit values.
			ic.vars[varDecl.Name] = 8
			zero := int64(0)
			ops = append(ops, Assign{Size: size, Target: varDecl.Name, Value: Arg{LiteralInt64: &zero}})
		default:
			panic(fmt.Sprintf("unsupported size %d, type %s", size, varDecl.Type))
		}
	} else if exprStmt, ok := node.(*ast.ExpressionStatement); ok {
		// We ignore the result of the expression.
		exprOps, _, _ := generateExpressionOps(ic, exprStmt.Expression)
		ops = append(ops, exprOps...)
	} else if retStmt, ok := node.(*ast.ReturnStatement); ok {
		if retStmt.Value != nil {
			// Return with value
			exprOps, resultArg, resultSize := generateExpressionOps(ic, retStmt.Value)
			ops = append(ops, exprOps...)
			ops = append(ops, Return{Size: resultSize, Value: &resultArg})
		} else {
			// Bare return
			ops = append(ops, Return{Value: nil})
		}
	} else if ifStmt, ok := node.(*ast.IfStatement); ok {
		ops = generateIfOps(ic, *ifStmt)
	} else if whileStmt, ok := node.(*ast.WhileStatement); ok {
		ops = generateWhileOps(ic, *whileStmt)
	} else if _, ok := node.(*ast.BreakStatement); ok {
		ops = append(ops, Jump{Goto: ic.breakLabel})
	} else if _, ok := node.(*ast.ContinueStatement); ok {
		ops = append(ops, Jump{Goto: ic.continueLabel})
	} else {
		panic(fmt.Sprintf("unknown statement type %v", node))
	}
	return ops
}

// generateExpressionOps generates a seequence of ops for a given expression.
// Returns a slice of ops, Arg representing the value (typically an intermediary), and the size of the value in bytes.
func generateExpressionOps(ic *IrContext, node ast.Expression) ([]Op, Arg, int) {
	if literal, ok := node.(*ast.Literal); ok {
		if literal.IntValue != nil {
			return []Op{}, Arg{LiteralInt: literal.IntValue}, 4
		} else if literal.Int64Value != nil {
			return []Op{}, Arg{LiteralInt64: literal.Int64Value}, 8
		} else if literal.StringValue != nil {
			return []Op{}, Arg{LiteralString: literal.StringValue}, 8
		} else if literal.BoolValue != nil {
			// Translate booleans into 32-bit integers.
			var intValue int32
			if *literal.BoolValue == true {
				intValue = 1
			} else {
				intValue = 0
			}
			return []Op{}, Arg{LiteralInt: &intValue}, 4
		} else {
			panic(fmt.Sprintf("Invalid literal: %v. Only int and string are currently supported", literal))
		}
	} else if assignment, ok := node.(*ast.Assignment); ok {
		ops, rvalueArg, rvalueSize := generateExpressionOps(ic, assignment.Value)
		ops = append(ops, Assign{Target: assignment.VariableName, Value: rvalueArg, Size: rvalueSize})
		return ops, rvalueArg, rvalueSize
	} else if call, ok := node.(*ast.FunctionCall); ok {
		return generateFunctionCallOps(ic, call)
	} else if ref, ok := node.(*ast.VariableReference); ok {
		return []Op{}, Arg{Variable: ref.Name}, ic.vars[ref.Name]
	} else if binOp, ok := node.(*ast.BinaryOperation); ok {
		leftOps, leftArg, leftSize := generateExpressionOps(ic, binOp.Left)
		rightOps, rightArg, rightSize := generateExpressionOps(ic, binOp.Right)
		if leftSize != rightSize {
			panic(fmt.Sprintf("%d:%d: size mismatch between operands of %s", binOp.Loc.Line, binOp.Loc.Col, binOp.Operator))
		}
		ops := append(leftOps, rightOps...)
		temp := ic.allocTemp(leftSize)
		resultSize := binaryOperationSize(binOp.Operator, leftSize)
		ops = append(ops, BinaryOp{Result: temp, Left: leftArg, Right: rightArg, Operation: binOp.Operator, Size: resultSize})
		return ops, Arg{Variable: temp}, leftSize
	} else if op, ok := node.(*ast.UnaryOperation); ok {
		ops, arg, operandSize := generateExpressionOps(ic, op.Operand)
		resultSize := unaryOperationSize(op.Operator, operandSize)
		temp := ic.allocTemp(resultSize)
		ops = append(ops, UnaryOp{Result: temp, Value: arg, Operation: op.Operator, Size: resultSize})
		return ops, Arg{Variable: temp}, resultSize
	}
	panic(fmt.Sprintf("Unknown expression type: %v", node))
}

func generateIfOps(ic *IrContext, stmt ast.IfStatement) []Op {
	ops := []Op{}

	if stmt.ElseBlock == nil {
		endLabel := ic.allocLabel()
		condOps, condArg, condSize := generateExpressionOps(ic, stmt.Condition)
		ops = append(ops, condOps...)
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

	size := getTypeSize(funcProto.ReturnType)
	temp := ic.allocTemp(size)
	ops = append(ops, Call{
		Result:    temp,
		Function:  call.FunctionName,
		Args:      args,
		ArgSizes:  sizes,
		NamedArgs: len(funcProto.Args),
		Size:      size,
	})

	return ops, Arg{Variable: temp}, size
}

func getTypeSize(typ types.Type) int {
	if _, ok := typ.(*types.PointerType); ok {
		return 8
	} else if typ.Equals(types.Bool) || typ.Equals(types.Int) {
		return 4
	} else if typ.Equals(types.Int64) || typ.Equals(types.String) {
		return 8
	}
	panic(fmt.Sprintf("unknown type %s", typ))
}

func unaryOperationSize(op string, operandSize int) int {
	if op == "&" {
		return WORD_SIZE
	}
	return operandSize
}

func binaryOperationSize(operator string, operandSize int) int {
	switch operator {
	case "==", "!=", "<", ">", "<=", ">=":
		return BOOL_SIZE
	}
	return operandSize
}
