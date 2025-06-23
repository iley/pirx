package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/parser"
)

type IrGenerator struct {
}

func NewIrGenerator() *IrGenerator {
	return &IrGenerator{}
}

func (g *IrGenerator) Generate(node *parser.Program) IrProgram {
	irp := IrProgram{}
	for _, function := range node.Functions {
		irFunc := g.generateFunction(function)
		irp.Functions = append(irp.Functions, irFunc)
	}
	return irp
}

func (g *IrGenerator) generateFunction(node *parser.Function) IrFunction {
	irfunc := IrFunction{
		Name:   node.Name,
		Params: []string{},
		Ops:    []Op{},
	}

	for _, param := range node.Params {
		irfunc.Params = append(irfunc.Params, param.Name)
	}

	nextTempIndex := 1
	if node.Body != nil {
		for _, stmt := range node.Body.Statements {
			ops := g.generateStatementOps(stmt, &nextTempIndex)
			irfunc.Ops = append(irfunc.Ops, ops...)
		}
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

func (g *IrGenerator) generateStatementOps(node parser.Statement, nextTempIndex *int) []Op {
	ops := []Op{}
	if node.VariableDeclaration != nil {
		// TODO: types.
		zero := 0
		ops = append(ops, Assign{Target: node.VariableDeclaration.Name, Value: Arg{LiteralInt: &zero}})
	} else if node.ExpressionStatement != nil {
		// We ignore the result of the expression.
		exprOps, _ := g.generateExpressionOps(node.ExpressionStatement.Expression, nextTempIndex)
		ops = append(ops, exprOps...)
	} else if node.ReturnStatement != nil {
		if node.ReturnStatement.Value != nil {
			// Return with value
			exprOps, resultArg := g.generateExpressionOps(*node.ReturnStatement.Value, nextTempIndex)
			ops = append(ops, exprOps...)
			ops = append(ops, Return{Value: &resultArg})
		} else {
			// Bare return
			ops = append(ops, Return{Value: nil})
		}
	} else {
		panic(fmt.Sprintf("unknown statement type %v", node))
	}
	return ops
}

func (g *IrGenerator) generateExpressionOps(node parser.Expression, nextTempIndex *int) ([]Op, Arg) {
	if node.Literal != nil {
		if node.Literal.IntValue != nil {
			return []Op{}, Arg{LiteralInt: node.Literal.IntValue}
		} else if node.Literal.StringValue != nil {
			return []Op{}, Arg{LiteralString: node.Literal.StringValue}
		} else {
			panic(fmt.Sprintf("Invalid literal: %v. Only int and string are currently supported", node.Literal))
		}
	} else if node.Assignment != nil {
		ops, rvalueArg := g.generateExpressionOps(node.Assignment.Value, nextTempIndex)
		ops = append(ops, Assign{Target: node.Assignment.VariableName, Value: rvalueArg})
		return ops, rvalueArg
	} else if node.FunctionCall != nil {
		ops := []Op{}
		args := []Arg{}
		for _, argNode := range node.FunctionCall.Args {
			subOps, subArg := g.generateExpressionOps(argNode, nextTempIndex)
			ops = append(ops, subOps...)
			args = append(args, subArg)
		}
		temp := allocTemp(nextTempIndex)
		ops = append(ops, Call{Result: temp, Function: node.FunctionCall.FunctionName, Args: args, Variadic: node.FunctionCall.Variadic})
		return ops, Arg{Variable: temp}
	} else if node.VariableReference != nil {
		return []Op{}, Arg{Variable: node.VariableReference.Name}
	} else if node.BinaryOperation != nil {
		leftOps, leftArg := g.generateExpressionOps(node.BinaryOperation.Left, nextTempIndex)
		rightOps, rightArg := g.generateExpressionOps(node.BinaryOperation.Right, nextTempIndex)
		ops := append(leftOps, rightOps...)
		temp := allocTemp(nextTempIndex)
		ops = append(ops, BinaryOp{Result: temp, Left: leftArg, Right: rightArg, Operation: node.BinaryOperation.Operator})
		return ops, Arg{Variable: temp}
	} else if node.UnaryOperation != nil {
		ops, arg := g.generateExpressionOps(node.UnaryOperation.Operand, nextTempIndex)
		temp := allocTemp(nextTempIndex)
		ops = append(ops, UnaryOp{Result: temp, Value: arg, Operation: node.UnaryOperation.Operator})
	}
	panic(fmt.Sprintf("Unknown expression type: %v", node))
}

func allocTemp(nextTempIndex *int) string {
	idx := *nextTempIndex
	*nextTempIndex++
	return fmt.Sprintf("$%d", idx)
}
