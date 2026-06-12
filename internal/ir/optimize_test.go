package ir

import "testing"

func TestFoldConstantsInvalidatesGlobalsAtCalls(t *testing.T) {
	// The callee can modify any global, so the known value of @g must be dropped.
	ops := []Op{
		Assign{Target: "@g", Value: intArg(0), Size: 4},
		Call{Function: "bump"},
		Call{Function: "use", Args: []CallArg{{Arg: Arg{Variable: "@g"}, Size: 4}}},
	}
	folded := foldConstants(ops)
	checkArgIsVariable(t, folded[2].(Call).Args[0].Arg, "@g")

	ops = []Op{
		Assign{Target: "@g", Value: intArg(0), Size: 4},
		ExternalCall{Function: "bump"},
		Call{Function: "use", Args: []CallArg{{Arg: Arg{Variable: "@g"}, Size: 4}}},
	}
	folded = foldConstants(ops)
	checkArgIsVariable(t, folded[2].(Call).Args[0].Arg, "@g")
}

func TestFoldConstantsInvalidatesGlobalsAtPointerStores(t *testing.T) {
	// "p" can point at @g even though &@g never appears in this function.
	ops := []Op{
		Assign{Target: "@g", Value: intArg(1), Size: 4},
		AssignByAddr{Target: Arg{Variable: "p"}, Value: intArg(5), Size: 4},
		Call{Function: "use", Args: []CallArg{{Arg: Arg{Variable: "@g"}, Size: 4}}},
	}
	folded := foldConstants(ops)
	checkArgIsVariable(t, folded[2].(Call).Args[0].Arg, "@g")
}

func TestFoldConstantsKeepsLocalsAcrossCallsAndPointerStores(t *testing.T) {
	// A local whose address is never taken cannot be touched by a call or a pointer
	// store, so it should still fold.
	ops := []Op{
		Assign{Target: "x", Value: intArg(7), Size: 4},
		Call{Function: "bump"},
		AssignByAddr{Target: Arg{Variable: "p"}, Value: intArg(5), Size: 4},
		Call{Function: "use", Args: []CallArg{{Arg: Arg{Variable: "x"}, Size: 4}}},
	}
	folded := foldConstants(ops)
	checkArgIsInt(t, folded[3].(Call).Args[0].Arg, 7)
}

func TestFoldUnaryMinusWrapsToOperandSize(t *testing.T) {
	// -(-128i8) overflows int8 and must wrap back to -128.
	ops := []Op{
		Assign{Target: "m", Value: intArg(-128), Size: 1},
		UnaryOp{Result: "n", Operation: "-", Value: Arg{Variable: "m"}, Size: 1},
	}
	folded := foldConstants(ops)
	checkAssignedInt(t, folded[1], -128)

	// Same for the minimal int32.
	ops = []Op{
		Assign{Target: "m", Value: intArg(-2147483648), Size: 4},
		UnaryOp{Result: "n", Operation: "-", Value: Arg{Variable: "m"}, Size: 4},
	}
	folded = foldConstants(ops)
	checkAssignedInt(t, folded[1], -2147483648)
}

func TestFoldConstantsSkipsDivisionByZero(t *testing.T) {
	// Folding the division would execute it inside the compiler and panic.
	ops := []Op{
		Assign{Target: "zero", Value: intArg(0), Size: 4},
		BinaryOp{Result: "x", Operation: "/", Left: intArg(10), Right: Arg{Variable: "zero"}, Size: 4, OperandSize: 4},
	}
	folded := foldConstants(ops)
	if _, ok := folded[1].(BinaryOp); !ok {
		t.Errorf("expected the division to remain a BinaryOp, got %s", folded[1])
	}
}

func intArg(value int64) Arg {
	return Arg{LiteralInt: &value}
}

func checkArgIsVariable(t *testing.T, arg Arg, name string) {
	t.Helper()
	if arg.Variable != name {
		t.Errorf("expected a reference to %s, got %s", name, arg)
	}
}

func checkArgIsInt(t *testing.T, arg Arg, value int64) {
	t.Helper()
	if arg.LiteralInt == nil || *arg.LiteralInt != value {
		t.Errorf("expected constant %d, got %s", value, arg)
	}
}

func checkAssignedInt(t *testing.T, op Op, value int64) {
	t.Helper()
	assign, ok := op.(Assign)
	if !ok {
		t.Fatalf("expected the op to fold into an Assign, got %s", op)
	}
	checkArgIsInt(t, assign.Value, value)
}
