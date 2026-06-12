package ir

import (
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
)

// The generator's diagnostics normally indicate inconsistencies that the
// typechecker should have caught, but they must still reach the caller
// instead of being silently dropped. We build the AST by hand (bypassing the
// typechecker) with a binary operation whose operands have different sizes.
func TestGenerateReturnsAccumulatedErrors(t *testing.T) {
	body := &ast.Block{
		Statements: []ast.Statement{
			&ast.ExpressionStatement{
				Expression: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(1),
					Operator: "+",
					Right:    ast.NewInt64Literal(2),
					Type:     ast.Int,
				},
			},
		},
	}
	program := &ast.Program{
		Functions: []ast.Function{{Name: "main", Body: body}},
		TypeTable: ast.NewTypeTable(),
	}

	_, errs := NewGenerator().Generate(program)
	if len(errs) == 0 {
		t.Fatal("expected Generate to return the size mismatch error, got no errors")
	}
	if !strings.Contains(errs[0].Error(), "size mismatch") {
		t.Errorf("expected a size mismatch error, got: %v", errs[0])
	}
}
