package ast

type FuncProto struct {
	Name         string
	ExternalName string
	Args         []FuncArg
	ReturnType   Type
	Variadic     bool
	// For external functions we need to follow C ABI.
	External bool
}

type FuncArg struct {
	Name string
	Typ  Type
}

func GetFunctionTable(program *Program) []FuncProto {
	protos := []FuncProto{}
	protos = append(protos, getBuiltins()...)

	for _, fn := range program.Functions {
		proto := FuncProto{
			Name:       fn.Name,
			Args:       []FuncArg{},
			ReturnType: fn.ReturnType,
			External:   fn.External,
		}
		for _, p := range fn.Args {
			proto.Args = append(proto.Args, FuncArg{p.Name, p.Type})
		}
		protos = append(protos, proto)
	}

	return protos
}

func getBuiltins() []FuncProto {
	return []FuncProto{
		{
			Name:     "printf",
			Args:     []FuncArg{{"fmt", String}},
			Variadic: true,
			External: true,
		},
		{
			Name:     "putchar",
			Args:     []FuncArg{{"ch", Int}},
			External: true,
		},
		{
			Name:         "dispose",
			ExternalName: "Pirx_Dispose",
			Args:         []FuncArg{{"p", Disposable}},
			External:     true,
		},
		{
			// This is not really a function, but we need a definition to make type checker happy.
			Name: "sizeof",
			Args: []FuncArg{{"expr", Any}},
		},
		{
			Name:         "resize",
			ExternalName: "Pirx_Slice_Resize",
			Args:         []FuncArg{{"slice", AnySlicePtr}, {"newsize", Int}},
			External:     true,
		},
		{
			Name:         "getptr",
			ExternalName: "Pirx_Slice_Ptr",
			Args:         []FuncArg{{"slice", AnySlice}},
			ReturnType:   VoidPtr, // Actual type inferred during typechecking.
			External:     true,
		},
		{
			Name:         "getsize",
			ExternalName: "Pirx_Slice_Size",
			Args:         []FuncArg{{"slice", AnySlice}},
			ReturnType:   Int,
			External:     true,
		},
		{
			Name:         "getcap",
			ExternalName: "Pirx_Slice_Cap",
			Args:         []FuncArg{{"slice", AnySlice}},
			ReturnType:   Int,
			External:     true,
		},
	}
}
