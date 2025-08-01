package types

import "github.com/iley/pirx/internal/ast"

func getBuiltins() []FuncProto {
	return []FuncProto{
		{
			Name:       "printf",
			Args:       []Arg{{"fmt", ast.String}},
			ReturnType: ast.Int,
			Variadic:   true,
			External:   true,
		},
		{
			Name:       "putchar",
			Args:       []Arg{{"ch", ast.Int}},
			ReturnType: ast.Int,
			External:   true,
		},
		{
			Name:         "dispose",
			ExternalName: "Pirx_Dispose",
			Args:         []Arg{{"p", ast.Disposable}},
			External:     true,
		},
	}
}
