package ir

import "fmt"

func Optimize(program IrProgram) IrProgram {
	optProgram := IrProgram{
		Functions: make([]IrFunction, len(program.Functions)),
	}

	for i, fn := range program.Functions {
		ops := foldConstants(fn.Ops)
		ops = removeDeadCode(ops)

		optFn := IrFunction{
			Name: fn.Name,
			Args: fn.Args,
			ArgSizes: fn.ArgSizes,
			Ops: ops,
		}
		optProgram.Functions[i] = optFn
	}

	return optProgram
}

func foldConstants(body []Op) []Op {
	// Map of variables that can potentially be modified via pointers.
	// We skip those as we cannot reliably determine whether they are modified at any point.
	leakedVars := make(map[string]bool)

	for _, op := range body {
		if unary, ok := op.(*UnaryOp); ok {
			if unary.Operation == "&" && unary.Value.Variable != ""  {
				leakedVars[unary.Value.Variable] = true
			}
		}
	}

	// Values known at compile time.
	knownValues := make(map[string]Arg)
	reset := func() {
		knownValues = make(map[string]Arg)
	}

	result := []Op{}
	for _, op := range body {
		if _, ok := op.(Anchor); ok {
			// Reset on encountering anchor (i.e. a label) because the program can potentially jump here.
			reset()
		}
		if _, ok := op.(Jump); ok {
			// Reset on unconditional jumps.
			reset()
		}
		if _, ok := op.(JumpUnless); ok {
			// Reset on conditional jumps.
			reset()
		}

		// Ignore the operations that write into variables for which pointers exist.
		// This does not conflict with the Anchor/Jump/JumpUnless checks below because those return "" for target.
		if !leakedVars[op.GetTarget()] {
			if assign, ok := op.(Assign); ok {
				// Assignment: if rvalue is known, track that.
				if value, ok := evalArg(knownValues, assign.Value); ok {
					knownValues[assign.Target] = value
				} else {
					// If we assign anything other that a constant to the variable, we no longer consider it known at compile time.
					delete(knownValues, assign.Target)
				}
			} else if binop, ok := op.(BinaryOp); ok {
				if value, ok := evalBinaryOp(knownValues, binop.Operation, binop.Left, binop.Right); ok {
					knownValues[binop.Result] = value
					// Replace the op with an assignment.
					op = Assign{
						Target: binop.Result,
						Value: value,
						Size: binop.Size,
					}
				} else {
					delete(knownValues, binop.Result)
				}
			} else if unop, ok := op.(UnaryOp); ok {
				if value, ok := evalUnaryOp(knownValues, unop.Operation, unop.Value); ok {
					knownValues[unop.Result] = value
					op = Assign{
						Target: unop.Result,
						Value: value,
						Size: unop.Size,
					}
				} else {
					delete(knownValues, unop.Result)
				}
			} else {
				delete(knownValues, op.GetTarget())
			}

			// TODO: Eval args in function call arguments.
		}

		result = append(result, op)
	}

	return result
}

func evalArg(knownValues map[string]Arg, arg Arg) (Arg, bool) {
	if arg.Variable != "" {
		if value, ok := knownValues[arg.Variable]; ok {
			return value, true
		}
		return Arg{}, false
	}
	// Everything else is a constant (al least currently).
	return arg, true
}

func evalBinaryOp(knownValues map[string]Arg, operation string, left, right Arg) (Arg, bool) {
	leftConst, leftOk := evalArg(knownValues, left)
	if !leftOk {
		return Arg{}, false
	}

	rightConst, rightOk := evalArg(knownValues, right)
	if !rightOk {
		return Arg{}, false
	}

	// We assume types are correct at this point.
	switch operation {
		// FIXME: Handle different int sizes!!!
		// Currently we assume there's now overflow.
	case "+":
		result := argIntValue(leftConst) + argIntValue(rightConst)
		return Arg{LiteralInt: &result}, true
	case "-":
		result := argIntValue(leftConst) - argIntValue(rightConst)
		return Arg{LiteralInt: &result}, true
	case "*":
		result := argIntValue(leftConst) * argIntValue(rightConst)
		return Arg{LiteralInt: &result}, true
	case "/":
		result := argIntValue(leftConst) / argIntValue(rightConst)
		return Arg{LiteralInt: &result}, true
	case "&&":
		result := argBoolValue(leftConst) && argBoolValue(rightConst)
		intResult := int64(0)
		if result {
			intResult = 1
		}
		return Arg{LiteralInt: &intResult}, true
	case "||":
		result := argBoolValue(leftConst) || argBoolValue(rightConst)
		intResult := int64(0)
		if result {
			intResult = 1
		}
		return Arg{LiteralInt: &intResult}, true
	}
	return Arg{}, false
}

func evalUnaryOp(knownValues map[string]Arg, operation string, value Arg) (Arg, bool) {
	constVal, ok := evalArg(knownValues, value)
	if !ok {
		return Arg{}, false
	}
	switch operation {
	/* FIXME: We're not folding negation yet, because the codegen doesn't handle negative literals correctly.
	case "-":
		result := -argIntValue(constVal)
		return Arg{LiteralInt: &result}, true
	*/
	case "!":
		result := !argBoolValue(constVal)
		intResult := int64(0)
		if result {
			intResult = 1
		}
		return Arg{LiteralInt: &intResult}, true
	}
	return Arg{}, false
}

func removeDeadCode(body []Op) []Op {
	refCount := make(map[string]int)

	for _, op := range body {
		for _, arg := range op.GetArgs() {
			if arg.Variable != "" {
				refCount[arg.Variable] += 1
			}
		}
	}

	result := []Op{}
	for _, op := range body {
		if assign, ok := op.(Assign); ok {
			// Skip ineffective assignments.
			if refCount[assign.Target] == 0 {
				continue
			}
		}

		result = append(result, op)
	}

	return result
}

func argIntValue(arg Arg) int64 {
	if arg.Zero {
		return 0
	} else if arg.LiteralInt != nil {
		return *arg.LiteralInt
	}
	panic(fmt.Errorf("arg does not have an integer value: %#v", arg))
}

func argBoolValue(arg Arg) bool {
	if arg.Zero {
		return false
	} else if arg.LiteralInt != nil {
		return *arg.LiteralInt != 0
	}
	panic(fmt.Errorf("arg does not have a boolean value: %#v", arg))
}
