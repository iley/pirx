package functions

import (
	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/types"
)

type Proto struct {
	Name       string
	Args       []Arg
	ReturnType types.Type
	Variadic   bool
}

type Arg struct {
	Name string
	Typ  types.Type
}

func GetFunctionTable(program *ast.Program) []Proto {
	protos := []Proto{}
	protos = append(protos, getBuiltins()...)

	for _, fn := range program.Functions {
		proto := Proto{
			Name:       fn.Name,
			Args:       []Arg{},
			ReturnType: fn.ReturnType,
		}
		for _, p := range fn.Args {
			proto.Args = append(proto.Args, Arg{p.Name, p.Type})
		}
		protos = append(protos, proto)
	}

	for _, fn := range program.ExternFunctions {
		proto := Proto{
			Name:       fn.Name,
			Args:       []Arg{},
			ReturnType: fn.ReturnType,
		}
		for _, p := range fn.Args {
			proto.Args = append(proto.Args, Arg{p.Name, p.Type})
		}
		protos = append(protos, proto)
	}

	return protos
}
