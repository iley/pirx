package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/functions"
	"github.com/iley/pirx/internal/parser"
)

type IrContext struct {
	nextTempIndex  int
	nextLabelIndex int
	breakLabel     string
	continueLabel  string
	funcs          map[string]functions.Proto
}

func (ic *IrContext) allocTemp() string {
	idx := ic.nextTempIndex
	ic.nextTempIndex++
	return fmt.Sprintf("$%d", idx)
}

func (ic *IrContext) allocLabel() string {
	idx := ic.nextLabelIndex
	ic.nextLabelIndex++
	return fmt.Sprintf("anchor%d", idx)
}

func Generate(node *parser.Program) IrProgram {
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

func generateFunction(ic *IrContext, node parser.Function) IrFunction {
	irfunc := IrFunction{
		Name:   node.Name,
		Params: []string{},
		Ops:    []Op{},
	}

	for _, param := range node.Params {
		irfunc.Params = append(irfunc.Params, param.Name)
	}

	// Create a new context for this function
	fic := *ic
	fic.nextTempIndex = 1

	irfunc.Ops = generateBlockOps(&fic, node.Body)

	// Add implicit return if the function doesn't end with a return
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

func generateBlockOps(ic *IrContext, block parser.Block) []Op {
	ops := []Op{}
	for _, stmt := range block.Statements {
		stmtOps := generateStatementOps(ic, stmt)
		ops = append(ops, stmtOps...)
	}
	return ops
}

func generateStatementOps(ic *IrContext, node parser.Statement) []Op {
	ops := []Op{}
	if varDecl, ok := node.(*parser.VariableDeclaration); ok {
		// TODO: types.
		zero := int64(0)
		ops = append(ops, Assign{Target: varDecl.Name, Value: Arg{LiteralInt: &zero}})
	} else if exprStmt, ok := node.(*parser.ExpressionStatement); ok {
		// We ignore the result of the expression.
		exprOps, _ := generateExpressionOps(ic, exprStmt.Expression)
		ops = append(ops, exprOps...)
	} else if retStmt, ok := node.(*parser.ReturnStatement); ok {
		if retStmt.Value != nil {
			// Return with value
			exprOps, resultArg := generateExpressionOps(ic, retStmt.Value)
			ops = append(ops, exprOps...)
			ops = append(ops, Return{Value: &resultArg})
		} else {
			// Bare return
			ops = append(ops, Return{Value: nil})
		}
	} else if ifStmt, ok := node.(*parser.IfStatement); ok {
		ops = generateIfOps(ic, *ifStmt)
	} else if whileStmt, ok := node.(*parser.WhileStatement); ok {
		ops = generateWhileOps(ic, *whileStmt)
	} else if _, ok := node.(*parser.BreakStatement); ok {
		ops = append(ops, Jump{Goto: ic.breakLabel})
	} else if _, ok := node.(*parser.ContinueStatement); ok {
		ops = append(ops, Jump{Goto: ic.continueLabel})
	} else {
		panic(fmt.Sprintf("unknown statement type %v", node))
	}
	return ops
}

func generateExpressionOps(ic *IrContext, node parser.Expression) ([]Op, Arg) {
	if literal, ok := node.(*parser.Literal); ok {
		if literal.IntValue != nil {
			return []Op{}, Arg{LiteralInt: literal.IntValue}
		} else if literal.StringValue != nil {
			return []Op{}, Arg{LiteralString: literal.StringValue}
		} else {
			panic(fmt.Sprintf("Invalid literal: %v. Only int and string are currently supported", literal))
		}
	} else if assignment, ok := node.(*parser.Assignment); ok {
		ops, rvalueArg := generateExpressionOps(ic, assignment.Value)
		ops = append(ops, Assign{Target: assignment.VariableName, Value: rvalueArg})
		return ops, rvalueArg
	} else if call, ok := node.(*parser.FunctionCall); ok {
		return generateFunctionCallOps(ic, call)
	} else if variableReference, ok := node.(*parser.VariableReference); ok {
		return []Op{}, Arg{Variable: variableReference.Name}
	} else if binaryOperation, ok := node.(*parser.BinaryOperation); ok {
		leftOps, leftArg := generateExpressionOps(ic, binaryOperation.Left)
		rightOps, rightArg := generateExpressionOps(ic, binaryOperation.Right)
		ops := append(leftOps, rightOps...)
		temp := ic.allocTemp()
		ops = append(ops, BinaryOp{Result: temp, Left: leftArg, Right: rightArg, Operation: binaryOperation.Operator})
		return ops, Arg{Variable: temp}
	} else if unaryOperation, ok := node.(*parser.UnaryOperation); ok {
		ops, arg := generateExpressionOps(ic, unaryOperation.Operand)
		temp := ic.allocTemp()
		ops = append(ops, UnaryOp{Result: temp, Value: arg, Operation: unaryOperation.Operator})
		return ops, Arg{Variable: temp}
	}
	panic(fmt.Sprintf("Unknown expression type: %v", node))
}

func generateIfOps(ic *IrContext, stmt parser.IfStatement) []Op {
	ops := []Op{}

	if stmt.ElseBlock == nil {
		endLabel := ic.allocLabel()
		condOps, condArg := generateExpressionOps(ic, stmt.Condition)
		ops = append(ops, condOps...)
		ops = append(ops, JumpUnless{Condition: condArg, Goto: endLabel})
		blockOps := generateBlockOps(ic, stmt.ThenBlock)
		ops = append(ops, blockOps...)
		ops = append(ops, Anchor{Label: endLabel})
	} else {
		elseLabel := ic.allocLabel()
		endLabel := ic.allocLabel()
		condOps, condArg := generateExpressionOps(ic, stmt.Condition)
		ops = append(ops, condOps...)
		ops = append(ops, JumpUnless{Condition: condArg, Goto: elseLabel})
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

func generateWhileOps(ic *IrContext, stmt parser.WhileStatement) []Op {
	prevBreakLabel := ic.breakLabel
	prevContinueLabel := ic.continueLabel

	ops := []Op{}
	ic.continueLabel = ic.allocLabel()
	ic.breakLabel = ic.allocLabel()

	ops = append(ops, Anchor{Label: ic.continueLabel})
	condOps, condArg := generateExpressionOps(ic, stmt.Condition)
	ops = append(ops, condOps...)
	ops = append(ops, JumpUnless{Condition: condArg, Goto: ic.breakLabel})
	blockOps := generateBlockOps(ic, stmt.Body)
	ops = append(ops, blockOps...)
	ops = append(ops, Jump{Goto: ic.continueLabel})
	ops = append(ops, Anchor{Label: ic.breakLabel})

	ic.breakLabel = prevBreakLabel
	ic.continueLabel = prevContinueLabel

	return ops
}

func generateFunctionCallOps(ic *IrContext, call *parser.FunctionCall) ([]Op, Arg) {
	ops := []Op{}
	args := []Arg{}
	for _, argNode := range call.Args {
		subOps, subArg := generateExpressionOps(ic, argNode)
		ops = append(ops, subOps...)
		args = append(args, subArg)
	}

	funcProto, ok := ic.funcs[call.FunctionName]
	if !ok {
		panic(fmt.Sprintf("unknown function %s", call.FunctionName))
	}

	if !funcProto.Variadic && len(args) != len(funcProto.Params) {
		panic(fmt.Sprintf("argument mismatch for function %s: expected %d arguments, got %d", call.FunctionName, len(args), len(funcProto.Params)))
	}

	temp := ic.allocTemp()
	ops = append(ops, Call{
		Result:    temp,
		Function:  call.FunctionName,
		Args:      args,
		NamedArgs: len(funcProto.Params),
	})

	return ops, Arg{Variable: temp}
}
