package functions

func getBuiltins() []Proto {
	return []Proto{
		{
			Name:       "printf",
			Args:       []Arg{{"fmt", "string"}},
			ReturnType: "int",
			Variadic:   true,
		},
		{
			Name:       "putchar",
			Args:       []Arg{{"ch", "int"}},
			ReturnType: "int",
		},
	}
}
