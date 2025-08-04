package desugar

import (
	"github.com/iley/pirx/internal/ast"
)

func Run(program *ast.Program) *ast.Program {
	return desugarProgram(program)
}

func desugarProgram(program *ast.Program) *ast.Program {
	result := *program
	result.Functions = make([]ast.Function, len(program.Functions))
	for i := range program.Functions {
		result.Functions[i] = *desugarFunction(&program.Functions[i])
	}
	return &result
}

func desugarFunction(function *ast.Function) *ast.Function {
	result := *function
	result.Body = desugarBlock(function.Body)
	return &result
}

func desugarBlock(block *ast.Block) *ast.Block {
	if block == nil {
		return nil
	}

	result := *block
	result.Statements = make([]ast.Statement, len(block.Statements))
	for i := range block.Statements {
		result.Statements[i] = desugarStatement(block.Statements[i])
	}

	return &result
}

func desugarStatement(stmt ast.Statement) ast.Statement {
	if forStmt, ok := stmt.(*ast.ForStatement); ok {
		return desugarForStatement(forStmt)
	}
	return stmt
}

// Desugar a `for init; cond; update { block }` into `{ init; while (cond) { block; update } }`.
func desugarForStatement(forStmt *ast.ForStatement) ast.Statement {
	statements := []ast.Statement{}

	if forStmt.Init != nil {
		desugaredInit := desugarStatement(forStmt.Init)
		statements = append(statements, desugaredInit)
	}

	whileStatements := make([]ast.Statement, 0, len(forStmt.Body.Statements)+1)
	for _, stmt := range forStmt.Body.Statements {
		whileStatements = append(whileStatements, desugarStatement(stmt))
	}
	if forStmt.Update != nil {
		desugaredUpdate := desugarExpression(forStmt.Update)
		updateStmt := &ast.ExpressionStatement{
			Loc:        desugaredUpdate.GetLocation(),
			Expression: desugaredUpdate,
		}
		whileStatements = append(whileStatements, updateStmt)
	}

	whileStmt := &ast.WhileStatement{
		Loc:       forStmt.Loc,
		Condition: desugarExpression(forStmt.Condition),
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

func desugarExpression(expr ast.Expression) ast.Expression {
	return expr
}
