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
	return irfunc
}

func (g *IrGenerator) generateStatementOps(node parser.Statement, nextTempIndex *int) []Op {
	ops := []Op{}
	if node.VariableDeclaration != nil {
		// TODO: types.
		zero := 0
		ops = append(ops, Assign{Target: node.VariableDeclaration.Name, Value: Arg{ImmediateInt: &zero}})
	} else if node.ExpressionStatement != nil {
		// We ignore the result of the expression.
		exprOps, _ := g.generateExpressionOps(node.ExpressionStatement.Expression, nextTempIndex)
		ops = append(ops, exprOps...)
	} else {
		panic(fmt.Sprintf("unknown statement type %v", node))
	}
	return ops
}

func (g *IrGenerator) generateExpressionOps(node parser.Expression, nextTempIndex *int) ([]Op, Arg) {
	if node.Literal != nil {
		// TODO: Support strings and other types.
		val := node.Literal.IntValue
		return []Op{}, Arg{ImmediateInt: &val}
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
		ops = append(ops, Call{Result: temp, Function: node.FunctionCall.FunctionName, Args: args})
		return ops, Arg{Variable: temp}
	}
	panic(fmt.Sprintf("Unknown expression type: %v", node))
}

func allocTemp(nextTempIndex *int) string {
	idx := *nextTempIndex
	*nextTempIndex++
	return fmt.Sprintf("$%d", idx)
}
