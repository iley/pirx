package types

import "github.com/iley/pirx/internal/ast"

func getBuiltins() []FuncProto {
	return []FuncProto{
		{
			Name:       "printf",
			Args:       []Arg{{"fmt", ast.String}},
			ReturnType: ast.Int,
			Variadic:   true,
		},
		{
			Name:       "putchar",
			Args:       []Arg{{"ch", ast.Int}},
			ReturnType: ast.Int,
		},
		{
			Name: "dispose",
			Args: []Arg{{"p", ast.VoidPtr}},
		},
	}
}
