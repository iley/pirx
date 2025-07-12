package functions

import "github.com/iley/pirx/internal/ast"

func getBuiltins() []Proto {
	return []Proto{
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
	}
}
