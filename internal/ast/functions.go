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
			Name:       "PirxString",
			Args:       []FuncArg{{"len", Int}, {"value", &PointerType{Int8}}},
			External:   true,
			ReturnType: String,
		},
		{
			Name:         "cstr",
			ExternalName: "PirxCStr",
			Args:         []FuncArg{{"str", String}},
			External:     true,
			ReturnType:   &PointerType{Int8},
		},
		{
			Name:         "printf",
			Args:         []FuncArg{{"fmt", String}},
			ExternalName: "PirxPrintf",
			Variadic:     true,
			External:     true,
		},
		{
			Name:     "putchar",
			Args:     []FuncArg{{"ch", Int}},
			External: true,
		},
		{
			Name:         "dispose",
			ExternalName: "PirxDispose",
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
			ExternalName: "PirxSliceResize",
			Args:         []FuncArg{{"slice", AnySlicePtr}, {"newsize", Int}},
			External:     true,
		},
		{
			Name:         "getptr",
			ExternalName: "PirxSlicePtr",
			Args:         []FuncArg{{"slice", AnySlice}},
			ReturnType:   VoidPtr, // Actual type inferred during typechecking.
			External:     true,
		},
		{
			Name:         "getsize",
			ExternalName: "PirxSliceSize",
			Args:         []FuncArg{{"slice", AnySlice}},
			ReturnType:   Int,
			External:     true,
		},
		{
			Name:         "getcap",
			ExternalName: "PirxSliceCap",
			Args:         []FuncArg{{"slice", AnySlice}},
			ReturnType:   Int,
			External:     true,
		},
		{
			Name:         "range",
			ExternalName: "PirxSliceRange",
			Args:         []FuncArg{{"slice", AnySlice}, {"start", Int}, {"end", Int}},
			ReturnType:   AnySlice,
			External:     true,
		},
		{
			Name:       "int",
			Args:       []FuncArg{{"value", Numeric}},
			ReturnType: Int,
			External:   true,
		},
		{
			Name:       "PirxIntFromInt",
			Args:       []FuncArg{{"value", Int}},
			ReturnType: Int,
			External:   true,
		},
		{
			Name:       "PirxIntFromInt8",
			Args:       []FuncArg{{"value", Int8}},
			ReturnType: Int,
			External:   true,
		},
		{
			Name:       "PirxIntFromInt64",
			Args:       []FuncArg{{"value", Int64}},
			ReturnType: Int,
			External:   true,
		},
		{
			Name:       "PirxIntFromFloat32",
			Args:       []FuncArg{{"value", Float32}},
			ReturnType: Int,
			External:   true,
		},
		{
			Name:       "PirxIntFromFloat64",
			Args:       []FuncArg{{"value", Float64}},
			ReturnType: Int,
			External:   true,
		},
		{
			Name:         "open",
			ExternalName: "PirxOpen",
			Args:         []FuncArg{{"path", String}},
			ReturnType:   File,
			External:     true,
		},
		{
			Name:         "readline",
			ExternalName: "PirxReadLine",
			Args:         []FuncArg{{"fp", File}},
			ReturnType:   String,
			External:     true,
		},
	}
}
