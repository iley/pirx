package checks

import "github.com/iley/pirx/internal/parser"

func Run(program *parser.Program) []error {
	varChecker := NewVariableChecker()
	program.Accept(varChecker)
	return varChecker.Errors()
}
