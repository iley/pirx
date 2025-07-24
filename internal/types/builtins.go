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
			Name:     "dispose",
			Args:     []Arg{{"p", ast.VoidPtr}},
			External: true,
		},
	}
}
