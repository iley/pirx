package desugar

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/util"
)

func Run(program *ast.Program) (*ast.Program, []error) {
	dc := &desugarContext{
		types:  program.TypeTable,
		errors: []error{},
	}
	result := desugarProgram(dc, program)
	return result, dc.errors
}

type desugarContext struct {
	types  *ast.TypeTable
	errors []error
}

func (dc *desugarContext) errorf(format string, args ...any) {
	dc.errors = append(dc.errors, fmt.Errorf(format, args...))
}

func desugarProgram(dc *desugarContext, program *ast.Program) *ast.Program {
	result := *program

	result.VariableDeclarations = make([]ast.VariableDeclaration, len(program.VariableDeclarations))
	for i := range program.VariableDeclarations {
		result.VariableDeclarations[i] = *desugarVariableDeclaration(dc, &program.VariableDeclarations[i])
	}

	result.Functions = make([]ast.Function, len(program.Functions))
	for i := range program.Functions {
		result.Functions[i] = *desugarFunction(dc, &program.Functions[i])
	}
	return &result
}

func desugarFunction(dc *desugarContext, function *ast.Function) *ast.Function {
	result := *function
	result.Body = desugarBlock(dc, function.Body)
	return &result
}

func desugarBlock(dc *desugarContext, block *ast.Block) *ast.Block {
	if block == nil {
		return nil
	}

	result := *block
	result.Statements = make([]ast.Statement, len(block.Statements))
	for i := range block.Statements {
		result.Statements[i] = desugarStatement(dc, block.Statements[i])
	}

	return &result
}

func desugarStatement(dc *desugarContext, stmt ast.Statement) ast.Statement {
	switch s := stmt.(type) {
	case *ast.VariableDeclaration:
		return desugarVariableDeclaration(dc, s)
	case *ast.ExpressionStatement:
		result := *s
		result.Expression = desugarExpression(dc, s.Expression)
		return &result
	case *ast.ReturnStatement:
		result := *s
		if s.Value != nil {
			result.Value = desugarExpression(dc, s.Value)
		}
		return &result
	case *ast.IfStatement:
		result := *s
		result.Condition = desugarExpression(dc, s.Condition)
		result.ThenBlock = *desugarBlock(dc, &s.ThenBlock)
		if s.ElseBlock != nil {
			elseBlock := desugarBlock(dc, s.ElseBlock)
			result.ElseBlock = elseBlock
		}
		return &result
	case *ast.WhileStatement:
		result := *s
		result.Condition = desugarExpression(dc, s.Condition)
		result.Body = *desugarBlock(dc, &s.Body)
		return &result
	case *ast.ForStatement:
		return desugarForStatement(dc, s)
	case *ast.BreakStatement:
		return s
	case *ast.ContinueStatement:
		return s
	case *ast.BlockStatement:
		result := *s
		result.Block = *desugarBlock(dc, &s.Block)
		return &result
	default:
		return stmt
	}
}

func desugarVariableDeclaration(dc *desugarContext, decl *ast.VariableDeclaration) *ast.VariableDeclaration {
	result := *decl
	if decl.Initializer != nil {
		result.Initializer = desugarExpression(dc, decl.Initializer)
	}
	return &result
}

// Desugar a `for init; cond; update { block }` into `{ init; while (cond) { block; update } }`.
func desugarForStatement(dc *desugarContext, forStmt *ast.ForStatement) ast.Statement {
	statements := []ast.Statement{}

	if forStmt.Init != nil {
		desugaredInit := desugarStatement(dc, forStmt.Init)
		statements = append(statements, desugaredInit)
	}

	whileStatements := make([]ast.Statement, 0, len(forStmt.Body.Statements)+1)
	for _, stmt := range forStmt.Body.Statements {
		whileStatements = append(whileStatements, desugarStatement(dc, stmt))
	}
	if forStmt.Update != nil {
		desugaredUpdate := desugarExpression(dc, forStmt.Update)
		updateStmt := &ast.ExpressionStatement{
			Loc:        desugaredUpdate.GetLocation(),
			Expression: desugaredUpdate,
		}
		whileStatements = append(whileStatements, updateStmt)
	}

	whileStmt := &ast.WhileStatement{
		Loc:       forStmt.Loc,
		Condition: desugarExpression(dc, forStmt.Condition),
		Body: ast.Block{
			Loc:        forStmt.Loc,
			Statements: whileStatements,
		},
	}

	statements = append(statements, whileStmt)

	return &ast.BlockStatement{
		Loc: forStmt.Loc,
		Block: ast.Block{
			Statements: statements,
		},
	}
}

func desugarExpression(dc *desugarContext, originalExpr ast.Expression) ast.Expression {
	switch expr := originalExpr.(type) {
	case *ast.Literal:
		return expr
	case *ast.Assignment:
		result := *expr
		result.Target = desugarExpression(dc, expr.Target)
		result.Value = desugarExpression(dc, expr.Value)
		return &result
	case *ast.VariableReference:
		return expr
	case *ast.FunctionCall:
		return desugarFunctionCall(dc, expr)
	case *ast.BinaryOperation:
		result := *expr
		result.Left = desugarExpression(dc, expr.Left)
		result.Right = desugarExpression(dc, expr.Right)
		return &result
	case *ast.UnaryOperation:
		result := *expr
		result.Operand = desugarExpression(dc, expr.Operand)
		return &result
	case *ast.FieldAccess:
		result := *expr
		result.Object = desugarExpression(dc, expr.Object)
		return &result
	case *ast.IndexExpression:
		result := *expr
		result.Array = desugarExpression(dc, expr.Array)
		result.Index = desugarExpression(dc, expr.Index)
		return &result
	case *ast.NewExpression:
		result := *expr
		if expr.Count != nil {
			result.Count = desugarExpression(dc, expr.Count)
		}
		return &result
	case *ast.PostfixOperator:
		result := *expr
		result.Operand = desugarExpression(dc, expr.Operand)
		return &result
	}
	panic(fmt.Errorf("%s: unknown expression type: %v", originalExpr.GetLocation(), originalExpr))
}

func desugarFunctionCall(dc *desugarContext, call *ast.FunctionCall) ast.Expression {
	// Recursively desugar function call arguments
	result := *call
	result.Args = make([]ast.Expression, len(call.Args))
	for i, arg := range call.Args {
		result.Args[i] = desugarExpression(dc, arg)
	}
	switch call.FunctionName {
	case "sizeof":
		return desugarSizeof(dc, &result)
	case "int":
		return desugarIntCast(&result)
	}
	return &result
}

func desugarSizeof(dc *desugarContext, call *ast.FunctionCall) ast.Expression {
	if len(call.Args) != 1 {
		dc.errorf("%s: size() takes exactly one argument, got %d", call.Loc, len(call.Args))
		return call
	}
	argType := call.Args[0].GetType()
	argSize, err := dc.types.GetSize(argType)
	if err != nil {
		dc.errorf("%s: invalid type %s, %v", call.Loc, argType, err)
	}
	return &ast.Literal{
		Loc:      call.Loc,
		IntValue: util.Int32Ptr(argSize),
		Type:     ast.Int,
	}
}

var intCasts = map[string]string{
	"int":     "Pirx_Int_From_Int",
	"int8":    "Pirx_Int_From_Int8",
	"int64":   "Pirx_Int_From_Int64",
	"float32": "Pirx_Int_From_Float32",
	"float64": "Pirx_Int_From_Float64",
}

func desugarIntCast(call *ast.FunctionCall) *ast.FunctionCall {
	argType := call.Args[0].GetType()
	resolvedFuncName, ok := intCasts[argType.String()]
	if !ok {
		panic(fmt.Errorf("could not resolve cast from %s to int", argType))
	}
	result := *call
	result.FunctionName = resolvedFuncName
	return &result
}
