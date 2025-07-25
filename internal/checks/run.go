package checks

import "github.com/iley/pirx/internal/ast"

func Run(program *ast.Program) (*ast.Program, []error) {
	typeChecker := NewTypeChecker(program)
	typedProgram := typeChecker.Check()
	return typedProgram, typeChecker.Errors()
}
