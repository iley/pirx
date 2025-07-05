package checks

import "github.com/iley/pirx/internal/ast"

func Run(program *ast.Program) []error {
	typeChecker := NewTypeChecker()
	typeChecker.CheckProgram(program)
	return typeChecker.Errors()
}
