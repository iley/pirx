package types

import (
	"github.com/iley/pirx/internal/ast"
)

type FuncProto struct {
	Name       string
	Args       []Arg
	ReturnType ast.Type
	Variadic   bool
	// For external functions we need to follow C ABI.
	External bool
}

type Arg struct {
	Name string
	Typ  ast.Type
}

func GetFunctionTable(program *ast.Program) []FuncProto {
	protos := []FuncProto{}
	protos = append(protos, getBuiltins()...)

	for _, fn := range program.Functions {
		proto := FuncProto{
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
		proto := FuncProto{
			Name:       fn.Name,
			Args:       []Arg{},
			ReturnType: fn.ReturnType,
			External: true,
		}
		for _, p := range fn.Args {
			proto.Args = append(proto.Args, Arg{p.Name, p.Type})
		}
		protos = append(protos, proto)
	}

	return protos
}
