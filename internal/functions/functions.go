package functions

import "github.com/iley/pirx/internal/parser"

type Proto struct {
	Name       string
	Args       []Arg
	ReturnType string
	Variadic   bool
}

type Arg struct {
	Name string
	Typ  string
}

func GetFunctionTable(program *parser.Program) []Proto {
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
