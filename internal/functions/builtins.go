package functions

func getBuiltins() []Proto {
	return []Proto{
		{
			Name:       "printf",
			Params:     []Param{{"fmt", "string"}},
			ReturnType: "int",
			Variadic:   true,
		},
		{
			Name:       "putchar",
			Params:     []Param{{"ch", "int"}},
			ReturnType: "int",
		},
	}
}
