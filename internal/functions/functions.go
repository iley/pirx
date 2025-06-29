package functions

import "github.com/iley/pirx/internal/parser"

type Proto struct {
	Name       string
	Params     []Param
	ReturnType string
	Variadic   bool
}

type Param struct {
	Name string
	Typ  string
}

func GetFunctionTable(program *parser.Program) []Proto {
	protos := []Proto{}
	protos = append(protos, getBuiltins()...)

	for _, fn := range program.Functions {
		proto := Proto{
			Name:       fn.Name,
			Params:     []Param{},
			ReturnType: fn.ReturnType,
		}
		for _, p := range fn.Params {
			proto.Params = append(proto.Params, Param{p.Name, p.Type})
		}
		protos = append(protos, proto)
	}

	return protos
}
