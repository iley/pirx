package functions

import "github.com/iley/pirx/internal/types"

func getBuiltins() []Proto {
	return []Proto{
		{
			Name:       "printf",
			Args:       []Arg{{"fmt", types.String}},
			ReturnType: types.Int,
			Variadic:   true,
		},
		{
			Name:       "putchar",
			Args:       []Arg{{"ch", types.Int}},
			ReturnType: types.Int,
		},
	}
}
