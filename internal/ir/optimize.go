package ir

import (
	"fmt"
	"slices"
)

func Optimize(program IrProgram) IrProgram {
	optProgram := IrProgram{
		Functions: make([]IrFunction, len(program.Functions)),
	}

	for i, fn := range program.Functions {
		ops := foldConstants(fn.Ops)
		// ops = reorderAssignments(ops)
		ops = removeIneffectiveAssignments(ops)

		optFn := IrFunction{
			Name:     fn.Name,
			Args:     fn.Args,
			ArgSizes: fn.ArgSizes,
			Ops:      ops,
		}
		optProgram.Functions[i] = optFn
	}

	return optProgram
}

type optimizationContext struct {
	// Values known at compile time.
	knownValues map[string]Arg
	// Map of variables that can potentially be modified via pointers.
	// We skip those as we cannot reliably determine whether they are modified at any point.
	leakedVars map[string]bool
}

func newOptimizationContext() *optimizationContext {
	return &optimizationContext{
		knownValues: make(map[string]Arg),
		leakedVars:  make(map[string]bool),
	}
}

func (oc *optimizationContext) reset() {
	oc.knownValues = make(map[string]Arg)
}

func (oc *optimizationContext) addKnownValue(varname string, value Arg) {
	if oc.leakedVars[varname] {
		return
	}
	oc.knownValues[varname] = value
}

func (oc *optimizationContext) getKnownValue(varname string) (Arg, bool) {
	arg, ok := oc.knownValues[varname]
	return arg, ok
}

func (oc *optimizationContext) invalidateKnownValue(varname string) {
	delete(oc.knownValues, varname)
}

func foldConstants(body []Op) []Op {
	oc := newOptimizationContext()

	for _, op := range body {
		if unary, ok := op.(UnaryOp); ok {
			if unary.Operation == "&" && unary.Value.Variable != "" {
				oc.leakedVars[unary.Value.Variable] = true
			}
		}
	}

	result := []Op{}
	for _, op := range body {
		if _, ok := op.(Anchor); ok {
			// Reset on encountering anchor (i.e. a label) because the program can potentially jump here.
			oc.reset()
		}
		if _, ok := op.(Jump); ok {
			// Reset on unconditional jumps.
			oc.reset()
		}
		if _, ok := op.(JumpUnless); ok {
			// Reset on conditional jumps.
			oc.reset()
		}

		// Ignore the operations that write into variables for which pointers exist.
		// This does not conflict with the Anchor/Jump/JumpUnless checks below because those return "" for target.
		if assign, ok := op.(Assign); ok {
			// Assignment: if rvalue is known, track that.
			if value, ok := evalArg(oc, assign.Value); ok {
				oc.addKnownValue(assign.Target, value)
			} else {
				// If we assign anything other that a constant to the variable, we no longer consider it known at compile time.
				oc.invalidateKnownValue(assign.Target)
			}
		} else if binop, ok := op.(BinaryOp); ok {
			if value, ok := evalBinaryOp(oc, binop.Operation, binop.Left, binop.Right, binop.OperandSize); ok {
				oc.addKnownValue(binop.Result, value)
				// Replace the op with an assignment.
				op = Assign{
					Target: binop.Result,
					Value:  value,
					Size:   binop.Size,
				}
			} else {
				oc.invalidateKnownValue(binop.Result)
			}
		} else if unop, ok := op.(UnaryOp); ok {
			if value, ok := evalUnaryOp(oc, unop.Operation, unop.Value); ok {
				oc.addKnownValue(unop.Result, value)
				op = Assign{
					Target: unop.Result,
					Value:  value,
					Size:   unop.Size,
				}
			} else {
				oc.invalidateKnownValue(unop.Result)
			}
		} else if call, ok := op.(Call); ok {
			args := make([]Arg, len(call.Args))
			for i, arg := range call.Args {
				constArg, ok := evalArg(oc, arg)
				if ok {
					args[i] = constArg
				} else {
					args[i] = arg
				}
			}
			call.Args = args
			op = call
			oc.invalidateKnownValue(call.Result)
		} else if call, ok := op.(ExternalCall); ok {
			args := make([]Arg, len(call.Args))
			for i, arg := range call.Args {
				constArg, ok := evalArg(oc, arg)
				if ok {
					args[i] = constArg
				} else {
					args[i] = arg
				}
			}
			call.Args = args
			op = call
			oc.invalidateKnownValue(call.Result)
		} else {
			oc.invalidateKnownValue(op.GetTarget())
		}

		result = append(result, op)
	}

	return result
}

func evalArg(oc *optimizationContext, arg Arg) (Arg, bool) {
	if arg.Variable != "" {
		if value, ok := oc.getKnownValue(arg.Variable); ok {
			return value, true
		}
		return Arg{}, false
	}
	// Everything else is a constant (al least currently).
	return arg, true
}

func performSizedIntegerArithmetic(operation string, leftVal, rightVal int64, size int) int64 {
	switch size {
	case 8: // int64
		switch operation {
		case "+":
			return leftVal + rightVal
		case "-":
			return leftVal - rightVal
		case "*":
			return leftVal * rightVal
		case "/":
			return leftVal / rightVal
		default:
			panic(fmt.Errorf("unsupported operation: %s", operation))
		}
	case 4: // int32
		left32 := int32(leftVal)
		right32 := int32(rightVal)
		var result32 int32
		switch operation {
		case "+":
			result32 = left32 + right32
		case "-":
			result32 = left32 - right32
		case "*":
			result32 = left32 * right32
		case "/":
			result32 = left32 / right32
		default:
			panic(fmt.Errorf("unsupported operation: %s", operation))
		}
		return int64(result32)
	case 1: // int8
		left8 := int8(leftVal)
		right8 := int8(rightVal)
		var result8 int8
		switch operation {
		case "+":
			result8 = left8 + right8
		case "-":
			result8 = left8 - right8
		case "*":
			result8 = left8 * right8
		case "/":
			result8 = left8 / right8
		default:
			panic(fmt.Errorf("unsupported operation: %s", operation))
		}
		return int64(result8)
	default:
		panic(fmt.Errorf("unsupported integer size: %d", size))
	}
}

func evalBinaryOp(oc *optimizationContext, operation string, left, right Arg, operandSize int) (Arg, bool) {
	leftConst, leftOk := evalArg(oc, left)
	if !leftOk {
		return Arg{}, false
	}

	rightConst, rightOk := evalArg(oc, right)
	if !rightOk {
		return Arg{}, false
	}

	// We assume types are correct at this point.
	switch operation {
	case "+", "-", "*", "/":
		result := performSizedIntegerArithmetic(operation, argIntValue(leftConst), argIntValue(rightConst), operandSize)
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

func evalUnaryOp(oc *optimizationContext, operation string, value Arg) (Arg, bool) {
	constVal, ok := evalArg(oc, value)
	if !ok {
		return Arg{}, false
	}
	switch operation {
	case "-":
		result := -argIntValue(constVal)
		return Arg{LiteralInt: &result}, true
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

func removeIneffectiveAssignments(body []Op) []Op {
	result := body

	// Keep removing ineffective assignments until none are left.
	for {
		passResult := removeIneffectiveAssignmentsPass(result)
		if len(result) == len(passResult) {
			break
		}
		result = passResult
	}

	return result
}

// Single pass of removing ineffective assignments.
func removeIneffectiveAssignmentsPass(body []Op) []Op {
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
			if refCount[assign.Target] == 0 && !IsGlobal(assign.Target) {
				continue
			}
		}

		result = append(result, op)
	}

	return result
}

// This is the first part of the intermediary elimination optimization.
// The idea is to replace pairs of operations like this:
//
// $1 = (operation A independent of x)
// x = $1
//
// With this:
//
// x = (operation A)
// $1 = x
//
// The idea is that ineffective assignment elimination will then get rid of "$1 = x" and we'll get rid of the intermediar and we'll get rid of the intermediary
func reorderAssignments(ops []Op) []Op {
	res := slices.Clone(ops)

	for i := 0; i < len(ops)-1; i++ {
		if canReorderOps(ops[i], ops[i+1]) {
			assign := ops[i+1].(Assign)
			res[i] = ReplaceTarget(ops[i], assign.Target)
			res[i+1] = Assign{Target: ops[i].GetTarget(), Value: Arg{Variable: assign.Target}, Size: assign.Size}
		}
	}

	return res
}

func canReorderOps(first, second Op) bool {
	assign, ok := second.(Assign)
	if !ok {
		return false
	}

	if assign.Value.Variable == ""  || first.GetTarget() != assign.Value.Variable {
		return false
	}

	for _, arg := range first.GetArgs() {
		if arg.Variable == assign.Target {
			// The variable is used in the expression.
			return false
		}
	}

	return true
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
