package codegen

import (
	"fmt"
	"io"

	"github.com/iley/pirx/internal/codegen/amd64_darwin"
	"github.com/iley/pirx/internal/parser"
)

type Target int

const (
	TargetARM64Darwin Target = iota
)

func TargetFromName(name string) (Target, error) {
	switch name {
	case "arm64-darwin":
		return TargetARM64Darwin, nil
	}
	return 0, fmt.Errorf("unknown target: %s", name)
}

func Generate(target Target, program *parser.Program, output io.Writer) error {
	var visitor parser.AstVisitor

	switch target {
	case TargetARM64Darwin:
		visitor = amd64_darwin.NewCodegenVisitor(output)
	default:
		return fmt.Errorf("unknown target: %d", target)
	}

	program.Accept(visitor)
	return nil
}
