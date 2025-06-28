package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/parser"
)

type IrContext struct {
	nextTempIndex  int
	nextLabelIndex int
	breakLabel     string
	continueLabel  string
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
	ic := IrContext{nextLabelIndex: 1}
	for _, function := range node.Functions {
		irFunc := generateFunction(&ic, function)
		irp.Functions = append(irp.Functions, irFunc)
	}
	return irp
}

func generateFunction(ic *IrContext, node *parser.Function) IrFunction {
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

	if node.Body != nil {
		irfunc.Ops = generateBlockOps(&fic, *node.Body)
	}

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
	if node.VariableDeclaration != nil {
		// TODO: types.
		zero := int64(0)
		ops = append(ops, Assign{Target: node.VariableDeclaration.Name, Value: Arg{LiteralInt: &zero}})
	} else if node.ExpressionStatement != nil {
		// We ignore the result of the expression.
		exprOps, _ := generateExpressionOps(ic, node.ExpressionStatement.Expression)
		ops = append(ops, exprOps...)
	} else if node.ReturnStatement != nil {
		if node.ReturnStatement.Value != nil {
			// Return with value
			exprOps, resultArg := generateExpressionOps(ic, *node.ReturnStatement.Value)
			ops = append(ops, exprOps...)
			ops = append(ops, Return{Value: &resultArg})
		} else {
			// Bare return
			ops = append(ops, Return{Value: nil})
		}
	} else if node.IfStatement != nil {
		ops = generateIfOps(ic, *node.IfStatement)
	} else if node.WhileStatement != nil {
		ops = generateWhileOps(ic, *node.WhileStatement)
	} else if node.BreakStatement != nil {
		ops = append(ops, Jump{Goto: ic.breakLabel})
	} else if node.ContinueStatement != nil {
		ops = append(ops, Jump{Goto: ic.continueLabel})
	} else {
		panic(fmt.Sprintf("unknown statement type %v", node))
	}
	return ops
}

func generateExpressionOps(ic *IrContext, node parser.Expression) ([]Op, Arg) {
	if node.Literal != nil {
		if node.Literal.IntValue != nil {
			return []Op{}, Arg{LiteralInt: node.Literal.IntValue}
		} else if node.Literal.StringValue != nil {
			return []Op{}, Arg{LiteralString: node.Literal.StringValue}
		} else {
			panic(fmt.Sprintf("Invalid literal: %v. Only int and string are currently supported", node.Literal))
		}
	} else if node.Assignment != nil {
		ops, rvalueArg := generateExpressionOps(ic, node.Assignment.Value)
		ops = append(ops, Assign{Target: node.Assignment.VariableName, Value: rvalueArg})
		return ops, rvalueArg
	} else if node.FunctionCall != nil {
		ops := []Op{}
		args := []Arg{}
		for _, argNode := range node.FunctionCall.Args {
			subOps, subArg := generateExpressionOps(ic, argNode)
			ops = append(ops, subOps...)
			args = append(args, subArg)
		}
		temp := ic.allocTemp()
		ops = append(ops, Call{Result: temp, Function: node.FunctionCall.FunctionName, Args: args, Variadic: node.FunctionCall.Variadic})
		return ops, Arg{Variable: temp}
	} else if node.VariableReference != nil {
		return []Op{}, Arg{Variable: node.VariableReference.Name}
	} else if node.BinaryOperation != nil {
		leftOps, leftArg := generateExpressionOps(ic, node.BinaryOperation.Left)
		rightOps, rightArg := generateExpressionOps(ic, node.BinaryOperation.Right)
		ops := append(leftOps, rightOps...)
		temp := ic.allocTemp()
		ops = append(ops, BinaryOp{Result: temp, Left: leftArg, Right: rightArg, Operation: node.BinaryOperation.Operator})
		return ops, Arg{Variable: temp}
	} else if node.UnaryOperation != nil {
		ops, arg := generateExpressionOps(ic, node.UnaryOperation.Operand)
		temp := ic.allocTemp()
		ops = append(ops, UnaryOp{Result: temp, Value: arg, Operation: node.UnaryOperation.Operator})
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
