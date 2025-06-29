package checks

import "github.com/iley/pirx/internal/parser"

func Run(program *parser.Program) []error {
	typeChecker := NewTypeChecker()
	typeChecker.CheckProgram(program)
	return typeChecker.Errors()
}
