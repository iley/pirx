package aarch64

import (
	_ "embed"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/codegen/common"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/util"
)

const (
	FUNC_CALL_REGISTERS = 8
	MAX_SP_OFFSET_X     = 504 // maximum offset from SP for 64-bit load/store instructions
	MAX_SP_OFFSET_WB    = 255 // maximum offset from SP for 32-bit/byte load/store instructions
)

type Features struct {
	VarargsOnStack       bool
	FuncLabelsUnderscore bool
}

type CodegenContext struct {
	features       Features
	stringLiterals map[string]string
	floatLiterals  map[float64]string

	// Function-specific.
	locals       map[string]int // name -> size
	frameSize    int
	functionName string
}

func Generate(irp ir.IrProgram, features Features) (asm.Program, error) {
	asmProgram := asm.Program{}

	// Map from string to a label in the data section.
	stringLiterals := make(map[string]string)
	for i, s := range common.GatherStrings(irp) {
		stringLiterals[s] = fmt.Sprintf(".Lstr%d", i)
	}

	// Map from float to a label in the data section.
	floatLiterals := make(map[float64]string)
	for i, f := range common.GatherFloats(irp) {
		floatLiterals[f] = fmt.Sprintf(".Lflt%d", i)
	}

	globalVariables := common.GatherGlobals(irp)

	cc := &CodegenContext{
		features:       features,
		stringLiterals: stringLiterals,
		floatLiterals:  floatLiterals,
	}

	for _, f := range irp.Functions {
		fn, err := generateFunction(cc, f)
		if err != nil {
			return asmProgram, fmt.Errorf("error when generating code for function %s: %w", f.Name, err)
		}
		asmProgram.Functions = append(asmProgram.Functions, fn)
	}

	asmProgram.StringLiterals = generateStringLiterals(stringLiterals)
	asmProgram.FloatLiterals = generateFloatLiterals(floatLiterals)
	asmProgram.GlobalVariables = generateGlobalVariables(globalVariables)

	return asmProgram, nil
}

func generateStringLiterals(literals map[string]string) []asm.StringLiteral {
	var result []asm.StringLiteral
	for str, label := range literals {
		result = append(result, asm.StringLiteral{Label: label, Text: str})
	}
	return result
}

func generateFloatLiterals(literals map[float64]string) []asm.FloatLiteral {
	var result []asm.FloatLiteral
	for val, label := range literals {
		result = append(result, asm.FloatLiteral{Label: label, Value: val})
	}
	return result
}

func generateGlobalVariables(globals map[string]int) []asm.GlobalVariable {
	var result []asm.GlobalVariable

	sortedGlobals := slices.Collect(maps.Keys(globals))
	slices.Sort(sortedGlobals)

	for _, name := range sortedGlobals {
		label := getGlobalLabel(name)
		size := globals[name]
		result = append(result, asm.GlobalVariable{Label: label, Size: size})
	}

	return result
}

func generateFunction(cc *CodegenContext, irfn ir.IrFunction) (asm.Function, error) {
	result := asm.Function{
		Name: irfn.Name,
	}

	// Map from local variable name to its size in bytes.
	lsizes := make(map[string]int)

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range irfn.Ops {
		if target := op.GetTarget(); target != "" {
			if ir.IsGlobal(target) {
				continue
			}
			if _, seen := lsizes[target]; seen {
				continue
			}
			lsizes[target] = op.GetSize()
		}
	}

	// Map from local variable name to its offset on the stack.
	locals := make(map[string]int)

	// Sort the locals by size to waste less stack space on alignment.
	sortedLocals := slices.Collect(maps.Keys(lsizes))
	slices.SortFunc(sortedLocals, func(a, b string) int {
		if lsizes[a] != lsizes[b] {
			return lsizes[a] - lsizes[b]
		}
		return strings.Compare(a, b)
	})

	// Generate offsets from SP for all locals.
	offset := 0
	for _, lname := range sortedLocals {
		// Ideally we'd want to align structs by the first element's size, but for simplicity we just default to 8 for anything larger than 8 bytes.
		// That is somewhat wasteful but who cares at this stage?
		align := min(8, lsizes[lname])
		offset = util.Align(offset, align)
		locals[lname] = offset
		offset += lsizes[lname]
	}

	// Space on the stack for local variables.
	// For now we're going to assume that all variables are 64-bit.
	// This does not include space for storing X29 and X30.
	// SP must always be aligned.
	frameSize := alignSP(offset)
	result.Lines = append(result.Lines,
		asm.Comment(fmt.Sprintf("frame size: %d bytes", frameSize)))

	// Space on stack for the registers wa save (plus padding).
	// * x29 is FP (frame pointer) by MacOS convention.
	// * x30 is LR (link register), it holds the return address.
	// * x19 is used by Pirx for the return value address.
	savedRegisters := 16

	// Calculate the size of the arguments block on the stack.
	argsBlockSize := 0
	for _, argSize := range irfn.ArgSizes {
		argsBlockSize += argSize
	}
	// Add one word for the saved x19.
	argsBlockSize += ast.WORD_SIZE
	// Include SP padding.
	argsBlockSize = alignSP(argsBlockSize)

	// Function arguments are already on the stack above our frame.
	// Calculate offsets for those.
	argOffset := frameSize + savedRegisters + argsBlockSize
	for i, arg := range irfn.Args {
		argOffset -= irfn.ArgSizes[i]
		locals[arg] = argOffset
	}

	result.Lines = append(result.Lines,
		// Save the registers.
		asm.Op3("sub", asm.SP, asm.SP, asm.Imm(savedRegisters)),
		asm.Op3("stp", asm.X29, asm.X30, asm.SP.AsDeref()),
		// Save frame start in X29.
		asm.Op2("mov", asm.X29, asm.SP),
		// Shift SP.
		asm.Op3("sub", asm.SP, asm.SP, asm.Imm(frameSize)))

	cc.locals = locals
	cc.frameSize = frameSize
	cc.functionName = irfn.Name

	for i, op := range irfn.Ops {
		result.Lines = append(result.Lines,
			asm.Comment(fmt.Sprintf("Op %d: %s", i, op.String())))
		lines, err := generateOp(cc, op)
		if err != nil {
			return result, err
		}
		result.Lines = append(result.Lines, lines...)
	}

	result.Lines = append(result.Lines,
		asm.Label(fmt.Sprintf(".L%s_exit", irfn.Name)),
		asm.Op3("add", asm.SP, asm.SP, asm.Imm(cc.frameSize)),
		asm.Op3("ldp", asm.X29, asm.X30, asm.SP.AsDeref()),
		asm.Op3("add", asm.SP, asm.SP, asm.Imm(savedRegisters)),
		asm.Op0("ret"))
	return result, nil
}

func generateOp(cc *CodegenContext, op ir.Op) ([]asm.Line, error) {
	if assign, ok := op.(ir.Assign); ok {
		return generateAssignment(cc, assign)
	} else if assign, ok := op.(ir.AssignByAddr); ok {
		return generateAssignmentByAddr(cc, assign)
	} else if call, ok := op.(ir.Call); ok {
		return generateFunctionCall(cc, call), nil
	} else if call, ok := op.(ir.ExternalCall); ok {
		return generateExternalFunctionCall(cc, call)
	} else if binop, ok := op.(ir.BinaryOp); ok {
		return generateBinaryOp(cc, binop)
	} else if unaryOp, ok := op.(ir.UnaryOp); ok {
		return generateUnaryOp(cc, unaryOp)
	} else if ret, ok := op.(ir.Return); ok {
		return generateReturn(cc, ret), nil
	} else if ret, ok := op.(ir.ExternalReturn); ok {
		return generateExternalReturn(cc, ret)
	} else if jump, ok := op.(ir.Jump); ok {
		return []asm.Line{asm.Op1("b", asm.Ref(fmt.Sprintf(".L%s_%s", cc.functionName, jump.Goto)))}, nil
	} else if jumpUnless, ok := op.(ir.JumpUnless); ok {
		var lines []asm.Line
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, jumpUnless.Size), jumpUnless.Size, jumpUnless.Condition)...)
		lines = append(lines, asm.Op2("cmp", asm.X0, asm.Imm(0)))
		lines = append(lines, asm.Op1("beq", asm.Ref(fmt.Sprintf(".L%s_%s", cc.functionName, jumpUnless.Goto))))
		return lines, nil
	} else if anchor, ok := op.(ir.Anchor); ok {
		return []asm.Line{asm.Label(fmt.Sprintf(".L%s_%s", cc.functionName, anchor.Label))}, nil
	} else {
		return nil, fmt.Errorf("unknown op type: %v", op)
	}
}

func generateAssignment(cc *CodegenContext, assign ir.Assign) ([]asm.Line, error) {
	var lines []asm.Line

	// TODO: Do we need to handle LiteralString here?
	if assign.Value.Variable != "" {
		if ir.IsGlobal(assign.Target) {
			// Load global's address into x1.
			lines = append(lines, generateAddressLoad(cc, "x1", ir.Arg{Variable: assign.Target})...)
			lines = append(lines, generateMemoryCopyToReg(cc, assign.Value, assign.Size, "x1", 0)...)
		} else {
			lines = append(lines, generateMemoryCopyToReg(cc, assign.Value, assign.Size, "sp", cc.locals[assign.Target])...)
		}
	} else if assign.Value.LiteralInt != nil {
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, assign.Size), assign.Target)...)
	} else if assign.Value.LiteralFloat != nil {
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, assign.Size), assign.Target)...)
	} else if assign.Value.Zero {
		if ir.IsGlobal(assign.Target) {
			// Do nothing.
			// This is a hack based on the fact that assignment of Arg{Zero: true} only happens in initialization, and globals are zero-initialized by the linker anyway.
		} else {
			lines = append(lines, generateRegisterLoad(cc, "x0", 8, assign.Value)...)
			offset := 0
			for offset < assign.Size {
				if assign.Size-offset >= 8 {
					lines = append(lines, generateStoreToLocalWithOffset(cc, "x0", assign.Target, offset)...)
					offset += 8
				} else if assign.Size-offset >= 4 {
					lines = append(lines, generateStoreToLocalWithOffset(cc, "w0", assign.Target, offset)...)
					offset += 4
				} else {
					lines = append(lines, generateStoreToLocalWithOffsetSized(cc, "w0", 1, assign.Target, offset)...)
					offset += 1
				}
			}
		}
	} else {
		return lines, fmt.Errorf("invalid rvalue in assignment: %v", assign.Value)
	}

	return lines, nil
}

func generateAssignmentByAddr(cc *CodegenContext, assign ir.AssignByAddr) ([]asm.Line, error) {
	var lines []asm.Line

	if assign.Value.Variable != "" {
		// Variable to variable.
		return generateMemoryCopyToReference(cc, assign.Value, assign.Size, assign.Target), nil
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)
		lines = append(lines, generateStoreByAddr(cc, registerByIndex(0, assign.Size), assign.Target, 0)...)
	} else if assign.Value.LiteralFloat != nil {
		// Assign float constant to variable.
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)
		lines = append(lines, generateStoreByAddr(cc, registerByIndex(0, assign.Size), assign.Target, 0)...)
	} else if assign.Value.Zero {
		return lines, fmt.Errorf("zero assignment by address not supported")
	} else {
		return lines, fmt.Errorf("invalid rvalue in assignment: %v", assign.Value)
	}

	return lines, nil
}

func generateFunctionCall(cc *CodegenContext, call ir.Call) []asm.Line {
	// For Pirx-native functions we use a simpler calling convention:
	//  * All arguments go on the stack.
	//  * Caller always allocates space for the return value.
	//  * The return value's address is passed in via x19.

	var lines []asm.Line
	offset := 0
	for _, arg := range call.Args {
		offset += arg.Size
		lines = append(lines, generateMemoryCopyToReg(cc, arg.Arg, arg.Size, "sp", -offset)...)
	}

	// Save x19 to the stack because it currently holds this function's result address.
	offset += ast.WORD_SIZE
	savedX19Offset := offset
	lines = append(lines, generateRegisterStore("x19", "sp", -offset)...)

	// Don't forget about stack alignment.
	offset = alignSP(offset)

	if call.Result != "" {
		// Store the result address in x19.
		lines = append(lines, generateAddressLoad(cc, "x19", ir.Arg{Variable: call.Result})...)
	}

	label := call.Function
	if cc.features.FuncLabelsUnderscore {
		label = "_" + label
	}

	lines = append(
		lines, asm.Op3("sub", asm.SP, asm.SP, asm.Imm(offset)),
		asm.Op1("bl", asm.Ref(label)),
		asm.Op3("add", asm.SP, asm.SP, asm.Imm(offset)))

	// Restore x19.
	x19Arg := asm.Arg{Reg: "sp", Offset: -savedX19Offset, Deref: true}
	lines = append(lines, asm.Op2("ldr", asm.Reg("x19"), x19Arg))

	return lines
}

// Generate a call to an external function using C ABI.
// ** WARNING! **
// We don't support the full C ABI here, just the bare minimum that we needed up to this point.
func generateExternalFunctionCall(cc *CodegenContext, call ir.ExternalCall) ([]asm.Line, error) {
	var lines []asm.Line
	remainingArgs := call.Args

	var nRegisterArgs int // how many args can go into registers
	if cc.features.VarargsOnStack {
		nRegisterArgs = call.NamedArgs
	} else {
		nRegisterArgs = len(call.Args)
	}

	nextIntRegister := 0   // X0
	nextFloatRegister := 0 // S0/D0
	for range nRegisterArgs {
		callArg := remainingArgs[0]
		arg := callArg.Arg
		argSize := callArg.Size

		// We can split a larger argument into two registers.
		needRegisters := 1
		if argSize > 8 {
			needRegisters = 2
		}

		// Check if we ran out of registers.
		if callArg.IsFloat {
			if nextFloatRegister+needRegisters > FUNC_CALL_REGISTERS {
				break
			}
		} else {
			if nextIntRegister+needRegisters > FUNC_CALL_REGISTERS {
				break
			}
		}

		switch argSize {
		case 1:
			reg32 := registerByIndex(nextIntRegister, 4)
			reg64 := registerByIndex(nextIntRegister, 8)
			lines = append(lines, generateRegisterLoad(cc, registerByIndex(nextIntRegister, argSize), argSize, arg)...)
			lines = append(lines, asm.Op2("sxtb", asm.Reg(reg64), asm.Reg(reg32)))
			nextIntRegister++
		case 4:
			if callArg.IsFloat {
				// Float argument - use S register
				sReg := fmt.Sprintf("s%d", nextFloatRegister)
				lines = append(lines, generateRegisterLoad(cc, sReg, argSize, arg)...)
				nextFloatRegister++
			} else {
				reg32 := registerByIndex(nextIntRegister, 4)
				reg64 := registerByIndex(nextIntRegister, 8)
				lines = append(lines, generateRegisterLoad(cc, registerByIndex(nextIntRegister, argSize), argSize, arg)...)
				lines = append(lines, asm.Op2("sxtw", asm.Reg(reg64), asm.Reg(reg32)))
				nextIntRegister++
			}
		case 8:
			if callArg.IsFloat {
				// Double argument - use D register
				dReg := fmt.Sprintf("d%d", nextFloatRegister)
				lines = append(lines, generateRegisterLoad(cc, dReg, argSize, arg)...)
				nextFloatRegister++
			} else {
				lines = append(lines, generateRegisterLoad(cc, registerByIndex(nextIntRegister, argSize), argSize, arg)...)
				nextIntRegister++
			}
		case 12:
			lines = append(lines, generateRegisterLoadWithOffset(cc, registerByIndex(nextIntRegister, 8), 8, arg, 0)...)
			lines = append(lines, generateRegisterLoadWithOffset(cc, registerByIndex(nextIntRegister+1, 4), 4, arg, 8)...)
			nextIntRegister += 2
		case 16:
			lines = append(lines, generateRegisterLoadWithOffset(cc, registerByIndex(nextIntRegister, 8), 8, arg, 0)...)
			lines = append(lines, generateRegisterLoadWithOffset(cc, registerByIndex(nextIntRegister+1, 8), 8, arg, 8)...)
			nextIntRegister += 2
		default:
			return lines, fmt.Errorf("unsupported external function argument size %d", argSize)
		}

		remainingArgs = remainingArgs[1:]
	}

	var spShift int

	// Everything else goes on the stack.
	if len(remainingArgs) > 0 {
		// First, move SP to allocate space for the args. Don't forget to align SP.
		// TODO: Walk the arguments and add their sizes up (including padding).
		spShift = alignSP(ast.WORD_SIZE * (len(remainingArgs)))

		// Then generate the stack pushes.
		for i, callArg := range remainingArgs {
			arg := callArg.Arg
			argSize := callArg.Size
			lines = append(lines, generateRegisterLoad(cc, registerByIndex(10, argSize), argSize, arg)...)
			// We extend all arguments to 64 bit.
			if argSize == 4 {
				// Sign extend 32-bit values to 64-bit
				lines = append(lines, asm.Op2("sxtw", asm.Reg("x10"), asm.Reg("w10")))
			}
			lines = append(lines, generateRegisterStore("x10", "sp", i*ast.WORD_SIZE-int(spShift))...)
		}
	}

	// Adjust SP just before making the call if we have arguments on the stack.
	if spShift > 0 {
		lines = append(lines, asm.Op3("sub", asm.SP, asm.SP, asm.Imm(spShift)))
	}

	label := call.Function
	if cc.features.FuncLabelsUnderscore {
		label = "_" + label
	}

	// Finally, call the function.
	lines = append(lines, asm.Op1("bl", asm.Ref(label)))

	// Don't forget to clean up if we put have arguments on the stack.
	if spShift > 0 {
		lines = append(lines, asm.Op3("add", asm.SP, asm.SP, asm.Imm(spShift)))
	}

	if call.Result != "" {
		// Store the result.
		lines = append(lines, generateExternalResultStore(cc, call.Size, call.Result)...)
	}

	return lines, nil
}

// TODO: Support unsigned operations.
func generateBinaryOp(cc *CodegenContext, binop ir.BinaryOp) ([]asm.Line, error) {
	var lines []asm.Line

	// Check if this is a floating point operation
	isFloatOp := len(binop.Operation) >= 2 && binop.Operation[len(binop.Operation)-1:] == "."

	if isFloatOp {
		// For floating point operations, load into FP registers via general-purpose registers
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, binop.OperandSize), binop.OperandSize, binop.Left)...)
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(1, binop.OperandSize), binop.OperandSize, binop.Right)...)

		// Move from general-purpose registers to floating point registers
		lines = append(lines, asm.Op2("fmov", asm.Reg("d0"), asm.Reg(registerByIndex(0, binop.OperandSize))))
		lines = append(lines, asm.Op2("fmov", asm.Reg("d1"), asm.Reg(registerByIndex(1, binop.OperandSize))))
	} else {
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, binop.OperandSize), binop.OperandSize, binop.Left)...)
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(1, binop.OperandSize), binop.OperandSize, binop.Right)...)
	}

	r0 := asm.Reg(registerByIndex(0, binop.OperandSize))
	r1 := asm.Reg(registerByIndex(1, binop.OperandSize))
	r2 := asm.Reg(registerByIndex(2, binop.OperandSize))

	switch binop.Operation {
	case "+":
		lines = append(lines, asm.Op3("add", r0, r0, r1))
	case "+.":
		lines = append(lines, asm.Op3("fadd", asm.Reg("d0"), asm.Reg("d0"), asm.Reg("d1")))
		lines = append(lines, asm.Op2("fmov", r0, asm.Reg("d0"))) // Move result back to general-purpose register
	case "-":
		lines = append(lines, asm.Op3("sub", r0, r0, r1))
	case "-.":
		lines = append(lines, asm.Op3("fsub", asm.Reg("d0"), asm.Reg("d0"), asm.Reg("d1")))
		lines = append(lines, asm.Op2("fmov", r0, asm.Reg("d0")))
	case "*":
		lines = append(lines, asm.Op3("mul", r0, r0, r1))
	case "*.":
		lines = append(lines, asm.Op3("fmul", asm.Reg("d0"), asm.Reg("d0"), asm.Reg("d1")))
		lines = append(lines, asm.Op2("fmov", r0, asm.Reg("d0")))
	case "/":
		lines = append(lines, asm.Op3("sdiv", r0, r0, r1))
	case "/.":
		lines = append(lines, asm.Op3("fdiv", asm.Reg("d0"), asm.Reg("d0"), asm.Reg("d1")))
		lines = append(lines, asm.Op2("fmov", r0, asm.Reg("d0")))
	case "%":
		lines = append(lines, asm.Op3("sdiv", r2, r0, r1))
		// msub r0, r2, r1, r0  (r0 = r0 - (r2 * r1))
		lines = append(lines, asm.Op4("msub", r0, r2, r1, r0))
	case "==":
		lines = append(lines, asm.Op2("cmp", r0, r1))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("eq")))
	case "!=":
		lines = append(lines, asm.Op2("cmp", r0, r1))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("ne")))
	case "<":
		lines = append(lines, asm.Op2("cmp", r0, r1))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("lt"))) // signed <
	case ">":
		lines = append(lines, asm.Op2("cmp", r0, r1))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("gt"))) // signed >
	case "<=":
		lines = append(lines, asm.Op2("cmp", r0, r1))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("le"))) // signed <=
	case ">=":
		lines = append(lines, asm.Op2("cmp", r0, r1))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("ge"))) // signed >=
	case "&&":
		lines = append(lines, asm.Op2("cmp", r0, asm.Imm(0)))
		lines = append(lines, asm.Op4("ccmp", r1, asm.Imm(0), asm.Imm(4), asm.Ref("ne")))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("ne")))
	case "||":
		lines = append(lines, asm.Op2("cmp", r0, asm.Imm(0)))
		lines = append(lines, asm.Op4("ccmp", r1, asm.Imm(0), asm.Imm(0), asm.Ref("eq")))
		lines = append(lines, asm.Op2("cset", r0, asm.Ref("ne")))
	default:
		return lines, fmt.Errorf("unsupported binary operation in aarch64-darwing codegen: %v", binop.Operation)
	}

	// Attention! This can be an implicit size cast!
	// A typical example where this happens is "boolean = (int64 == int64)".
	lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, binop.Size), binop.Result)...)
	return lines, nil
}

func generateUnaryOp(cc *CodegenContext, op ir.UnaryOp) ([]asm.Line, error) {
	var lines []asm.Line

	switch op.Operation {
	case "!":
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, op.Size), op.Size, op.Value)...)
		lines = append(lines, asm.Op2("cmp", asm.X0, asm.Imm(0)))
		lines = append(lines, asm.Op2("cset", asm.X0, asm.Ref("eq")))
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, op.Size), op.Result)...)
	case "-":
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, op.Size), op.Size, op.Value)...)
		reg := asm.Reg(registerByIndex(0, op.Size))
		lines = append(lines, asm.Op2("neg", reg, reg))
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, op.Size), op.Result)...)
	case "&":
		lines = append(lines, generateAddressLoad(cc, "x0", op.Value)...)
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, op.Size), op.Result)...)
	case "*":
		lines = append(lines, generateMemoryCopyFromRef(cc, op.Value, op.Size, op.Result)...)
	default:
		return lines, fmt.Errorf("unsupported unary operation %s", op.Operation)
	}

	return lines, nil
}

func generateReturn(cc *CodegenContext, ret ir.Return) []asm.Line {
	var lines []asm.Line

	// Copy the return value into the slot provided by the caller via x19.
	if ret.Value != nil {
		lines = append(lines, generateMemoryCopyToReg(cc, *ret.Value, ret.Size, "x19", 0)...)
	}

	lines = append(lines, asm.Op1("b", asm.Ref(fmt.Sprintf(".L%s_exit", cc.functionName))))
	return lines
}

// Return from an externally referenced function i.e. C ABI compatible.
func generateExternalReturn(cc *CodegenContext, ret ir.ExternalReturn) ([]asm.Line, error) {
	var lines []asm.Line

	exitBranch := asm.Op1("b", asm.Ref(fmt.Sprintf(".L%s_exit", cc.functionName)))

	if ret.Value == nil {
		lines = append(lines, exitBranch)
		return lines, nil
	}

	// TODO: Support floats in non-variadic calls.

	switch ret.Size {
	case 4:
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, ret.Size), ret.Size, *ret.Value)...)
		lines = append(lines, asm.Op2("sxtw", asm.X0, asm.W0))
	case 8:
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, ret.Size), ret.Size, *ret.Value)...)
	case 12, 16:
		// Load first 8 bytes into x0...
		lines = append(lines, generateRegisterLoadWithOffset(cc, "x0", 8, *ret.Value, 0)...)
		// And the rest into x1/w1.
		lines = append(lines, generateRegisterLoadWithOffset(cc, registerByIndex(1, ret.Size-8), ret.Size-8, *ret.Value, 8)...)
	default:
		// TODO: Support sizes over 16 bytes via indirect return (caller allocates space and passess the address in x19).
		return lines, fmt.Errorf("unsupported return value size: %d", ret.Size)
	}

	lines = append(lines, exitBranch)
	return lines, nil
}

func generateExternalResultStore(cc *CodegenContext, size int, target string) []asm.Line {
	if size > 16 {
		// TODO: Returns over 16 bytes are done via a caller-allocated buffer referenced by x8.
		// Once that is supported on the call side, we can just remove the check here.
		panic("returning values larger than 16 bytes from externa functions is not currently supported")
	}

	var lines []asm.Line
	offset := 0
	for offset < size {
		// The return value can be split between X0 and X1.
		var retReg int
		if offset < 8 {
			retReg = 0 // X0
		} else {
			retReg = 1 // X1
		}

		if size-offset >= 8 {
			lines = append(lines, generateStoreToLocalWithOffset(cc, registerByIndex(retReg, 8), target, offset)...)
			offset += 8
		} else if size-offset >= 4 {
			lines = append(lines, generateStoreToLocalWithOffset(cc, registerByIndex(retReg, 4), target, offset)...)
			offset += 4
			// Skip the shfit if we're either at the end or at register boundary.
			if size > offset && offset != 8 {
				lines = append(lines, asm.Op3("lsr", asm.Reg(fmt.Sprintf("x%d", retReg)), asm.Reg(fmt.Sprintf("x%d", retReg)), asm.Imm(32)))
			}
		} else {
			// Copy one byte at a time as we don't support half-word access currently.
			lines = append(lines, generateStoreToLocalWithOffset(cc, registerByIndex(retReg, 1), target, offset)...)
			offset += 1
			if size > offset {
				lines = append(lines, asm.Op3("lsr", asm.Reg(fmt.Sprintf("x%d", retReg)), asm.Reg(fmt.Sprintf("x%d", retReg)), asm.Imm(8)))
			}
		}
	}

	return lines
}

// generateRegisterLoad generates code for loading a value into a register by its name and size.
// trashes X9.
func generateRegisterLoad(cc *CodegenContext, reg string, regSize int, arg ir.Arg) []asm.Line {
	return generateRegisterLoadWithOffset(cc, reg, regSize, arg, 0)
}

func generateGlobalVariableLoadWithOffset(reg string, regSize int, variable string, offset int) []asm.Line {
	var lines []asm.Line
	label := getGlobalLabel(variable)

	lines = append(lines, asm.Op2("adrp", asm.X9, asm.Ref(label).WithPage()))

	if offset == 0 {
		lines = append(lines, asm.Op3("add", asm.X9, asm.X9, asm.Ref(label).WithPageOff()))
	} else {
		// TODO: We could do a single operatation here instead of two: add x9, x9, label@PAGEOFF + offset
		lines = append(lines, asm.Op3("add", asm.X9, asm.X9, asm.Ref(label).WithPageOff()),
			asm.Op3("add", asm.X9, asm.X9, asm.Imm(offset)))
	}

	if regSize == 1 {
		lines = append(lines, asm.Op2("ldrsb", asm.Reg(reg), asm.X9.AsDeref()))
	} else {
		lines = append(lines, asm.Op2("ldr", asm.Reg(reg), asm.X9.AsDeref()))
	}

	return lines
}

func generateLocalVariableLoadWithOffset(cc *CodegenContext, reg string, regSize int, variable string, offset int) []asm.Line {
	var lines []asm.Line
	fullOffset := int64(cc.locals[variable]) + int64(offset)

	maxOffset := MAX_SP_OFFSET_X
	if regSize == 4 || regSize == 1 {
		maxOffset = MAX_SP_OFFSET_WB
	}

	if fullOffset <= int64(maxOffset) {
		spArg := asm.Arg{Reg: "sp", Offset: int(fullOffset), Deref: true}
		if regSize == 1 {
			lines = append(lines, asm.Op2("ldrsb", asm.Reg(reg), spArg))
		} else {
			lines = append(lines, asm.Op2("ldr", asm.Reg(reg), spArg))
		}
	} else {
		lines = append(lines, generateLiteralLoad("x9", 8, fullOffset)...)
		lines = append(lines, asm.Op3("add", asm.X9, asm.SP, asm.X9))
		if regSize == 1 {
			lines = append(lines, asm.Op2("ldrsb", asm.Reg(reg), asm.X9.AsDeref()))
		} else {
			lines = append(lines, asm.Op2("ldr", asm.Reg(reg), asm.X9.AsDeref()))
		}
	}

	return lines
}

func generateStringLiteralLoad(cc *CodegenContext, reg string, literal string) []asm.Line {
	label := cc.stringLiterals[literal]
	return []asm.Line{
		asm.Op2("adrp", asm.Reg(reg), asm.Ref(label).WithPage()),
		asm.Op3("add", asm.Reg(reg), asm.Reg(reg), asm.Ref(label).WithPageOff()),
	}
}

func generateRegisterLoadWithOffset(cc *CodegenContext, reg string, regSize int, arg ir.Arg, offset int) []asm.Line {
	if arg.Variable != "" {
		if ir.IsGlobal(arg.Variable) {
			return generateGlobalVariableLoadWithOffset(reg, regSize, arg.Variable, offset)
		} else {
			return generateLocalVariableLoadWithOffset(cc, reg, regSize, arg.Variable, offset)
		}
	} else if arg.LiteralInt != nil {
		if offset != 0 {
			panic("cannot load a literal int64 with offset")
		}
		return generateLiteralLoad(reg, regSize, *arg.LiteralInt)
	} else if arg.LiteralFloat != nil {
		if offset != 0 {
			panic("cannot load a literal float64 with offset")
		}
		return generateFloatLiteralLoad(cc, reg, regSize, *arg.LiteralFloat)
	} else if arg.LiteralString != nil {
		if offset != 0 {
			panic("cannot load a literal string with offset")
		}
		if regSize != ast.WORD_SIZE {
			panic(fmt.Errorf("cannot load a string literal into register of size %d", regSize))
		}
		return generateStringLiteralLoad(cc, reg, *arg.LiteralString)
	} else if arg.Zero {
		return []asm.Line{asm.Op2("mov", asm.Reg(reg), asm.Imm(0))}
	} else {
		panic(fmt.Errorf("invalid arg in code generation: %v", arg))
	}
}

func generateAddressLoad(cc *CodegenContext, reg string, arg ir.Arg) []asm.Line {
	if arg.Variable == "" {
		panic(fmt.Errorf("can only load address of variables, got %s", arg))
	}

	var lines []asm.Line
	regArg := asm.Reg(reg)

	if ir.IsGlobal(arg.Variable) {
		label := getGlobalLabel(arg.Variable)
		lines = append(lines, asm.Op2("adrp", regArg, asm.Ref(label).WithPage()))
		lines = append(lines, asm.Op3("add", regArg, regArg, asm.Ref(label).WithPageOff()))
	} else {
		offset := int64(cc.locals[arg.Variable])
		lines = append(lines, asm.Op3("add", regArg, asm.SP, asm.Imm(int(offset))))
	}

	return lines
}

// TODO: Store set of currently used registers in the context.
// Trashes X1 and X9.
func generateStoreToVariable(cc *CodegenContext, reg string, target string) []asm.Line {
	var lines []asm.Line

	if ir.IsGlobal(target) {
		if reg == "x1" {
			panic("cannot do generateStoreToVariable for x1")
		}
		lines = append(lines, generateAddressLoad(cc, "x1", ir.Arg{Variable: target})...)
		lines = append(lines, generateRegisterStore(reg, "x1", 0)...)
	} else {
		lines = append(lines, generateStoreToLocalWithOffset(cc, reg, target, 0)...)
	}

	return lines
}

func generateStoreToLocalWithOffset(cc *CodegenContext, reg string, target string, offset int) []asm.Line {
	fullOffset := cc.locals[target] + offset
	return generateRegisterStore(reg, "sp", fullOffset)
}

// generateStoreToLocalWithOffsetSized stores with explicit size (for cases where register name doesn't indicate the operation size)
func generateStoreToLocalWithOffsetSized(cc *CodegenContext, reg string, regSize int, target string, offset int) []asm.Line {
	fullOffset := cc.locals[target] + offset
	return generateRegisterStoreSized(reg, regSize, "sp", fullOffset)
}

// generateRegisterStore generates a store to memory location identified by base register and offset.
func generateRegisterStore(reg string, baseReg string, offset int) []asm.Line {
	regSize := registerSizeFromName(reg)
	return generateRegisterStoreSized(reg, regSize, baseReg, offset)
}

// generateStoreByAddr generates code for storing a register through a pointer.
// Loads the pointer address into a register and stores the value through it.
func generateStoreByAddr(cc *CodegenContext, reg string, target ir.Arg, offset int) []asm.Line {
	var lines []asm.Line

	// Load the destination address.
	lines = append(lines, generateRegisterLoad(cc, "x1", ast.WORD_SIZE, target)...)
	addrReg := asm.Reg("x1")
	srcReg := asm.Reg(reg)
	regSize := registerSizeFromName(reg)

	// Add offset to the address.
	if offset != 0 {
		lines = append(lines, asm.Op3("add", addrReg, addrReg, asm.Imm(offset)))
	}

	if regSize == 1 {
		lines = append(lines, asm.Op2("strb", srcReg, addrReg.AsDeref()))
	} else {
		lines = append(lines, asm.Op2("str", srcReg, addrReg.AsDeref()))
	}

	return lines
}

// Copy `size` bytes from `source` to the memory location identified by a register and an offset.
// Trashes x0.
func generateMemoryCopyToReg(cc *CodegenContext, source ir.Arg, size int, baseReg string, baseOffset int) []asm.Line {
	var lines []asm.Line
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "x0", 8, source, offset)...)
			lines = append(lines, generateRegisterStore("x0", baseReg, baseOffset+offset)...)
			offset += 8
		} else if size-offset >= 4 {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "w0", 4, source, offset)...)
			lines = append(lines, generateRegisterStore("w0", baseReg, baseOffset+offset)...)
			offset += 4
		} else {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "w0", 1, source, offset)...)
			lines = append(lines, generateRegisterStore("w0", baseReg, baseOffset+offset)...)
			offset += 1
		}
	}
	return lines
}

// Copy `size` bytes from `source` to a memory location based that `targetRef` points to.
// Trashes x0.
func generateMemoryCopyToReference(cc *CodegenContext, source ir.Arg, size int, targetRef ir.Arg) []asm.Line {
	lines := []asm.Line{}
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "x0", 8, source, offset)...)
			lines = append(lines, generateStoreByAddr(cc, "x0", targetRef, offset)...)
			offset += 8
		} else if size-offset >= 4 {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "w0", 4, source, offset)...)
			lines = append(lines, generateStoreByAddr(cc, "w0", targetRef, offset)...)
			offset += 4
		} else {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "w0", 1, source, offset)...)
			lines = append(lines, generateStoreByAddr(cc, "w0", targetRef, offset)...)
			offset += 1
		}
	}
	return lines
}

// Copy `size` bytes from memory location `sourceRef` points to, to the local variable `target`.
// Trashes x0 and x1.
func generateMemoryCopyFromRef(cc *CodegenContext, sourceRef ir.Arg, size int, target string) []asm.Line {
	var lines []asm.Line

	// Load the source address to x1.
	sourceAddrReg := 1
	lines = append(lines, generateRegisterLoad(cc, registerByIndex(sourceAddrReg, ast.WORD_SIZE), ast.WORD_SIZE, sourceRef)...)

	offset := 0
	for offset < size {
		if size-offset >= 8 {
			lines = append(lines, asm.Op2("ldr", asm.X0, asm.DerefWithOffset(asm.X1, offset)))
			lines = append(lines, generateStoreToLocalWithOffset(cc, "x0", target, offset)...)
			offset += 8
		} else if size-offset >= 4 {
			lines = append(lines, asm.Op2("ldr", asm.W0, asm.DerefWithOffset(asm.X1, offset)))
			lines = append(lines, generateStoreToLocalWithOffset(cc, "w0", target, offset)...)
			offset += 4
		} else {
			lines = append(lines, asm.Op2("ldrb", asm.W0, asm.DerefWithOffset(asm.X1, offset)))
			lines = append(lines, generateStoreToLocalWithOffsetSized(cc, "w0", 1, target, offset)...)
			offset += 1
		}
	}

	return lines
}

func generateLiteralLoad(reg string, regSize int, val int64) []asm.Line {
	var lines []asm.Line

	lines = append(lines, asm.Op2("mov", asm.Reg(reg), asm.Imm(int(val&0xffff))))

	if (val>>16)&0xffff != 0 {
		lines = append(lines, asm.Op3("movk", asm.Reg(reg), asm.Imm(int((val>>16)&0xffff)), asm.LSL(16)))
	}

	if regSize == 8 {
		if (val>>32)&0xffff != 0 {
			lines = append(lines, asm.Op3("movk", asm.Reg(reg), asm.Imm(int((val>>32)&0xffff)), asm.LSL(32)))
		}
		if (val>>48)&0xffff != 0 {
			lines = append(lines, asm.Op3("movk", asm.Reg(reg), asm.Imm(int((val>>48)&0xffff)), asm.LSL(48)))
		}
	}

	return lines
}

func generateFloatLiteralLoad(cc *CodegenContext, reg string, regSize int, literal float64) []asm.Line {
	label := cc.floatLiterals[literal]
	var lines []asm.Line

	// Load the address of the float literal into x9 (temporary register)
	lines = append(lines, asm.Op2("adrp", asm.Reg("x9"), asm.Ref(label).WithPage()))
	lines = append(lines, asm.Op3("add", asm.Reg("x9"), asm.Reg("x9"), asm.Ref(label).WithPageOff()))

	// Load the actual float value from memory
	switch regSize {
	case 8, 4:
		lines = append(lines, asm.Op2("ldr", asm.Reg(reg), asm.Reg("x9").AsDeref()))
	default:
		panic(fmt.Errorf("unsupported register size for float literal: %d", regSize))
	}

	return lines
}

func registerByIndex(index, size int) string {
	switch size {
	case 1, 4:
		// wX is used for accessing bytes as well.
		return fmt.Sprintf("w%d", index)
	case 8:
		return fmt.Sprintf("x%d", index)
	default:
		panic(fmt.Errorf("invalid register size %d", size))
	}
}

/*
func fPRegisterByIndex(index, size int) string {
	if size == 4 {
		return fmt.Sprintf("s%d", index)
	}
	if size == 8 {
		return fmt.Sprintf("d%d", index)
	}
	panic(fmt.Errorf("invalid floating point register size: %d", size))
}
*/

func registerSizeFromName(reg string) int {
	if len(reg) == 0 {
		panic("empty register name")
	}
	switch reg[0] {
	case 'x', 'd':
		return 8
	case 'w', 's':
		return 4
	case 'h':
		return 2
	case 'b':
		return 1
	default:
		panic(fmt.Errorf("unknown register type: %s", reg))
	}
}

// generateRegisterStoreSized generates a store with explicit size (for edge cases)
func generateRegisterStoreSized(reg string, regSize int, baseReg string, offset int) []asm.Line {
	var lines []asm.Line
	regArg := asm.Reg(reg)

	// Different instruction types have different offset limits
	maxOffset := MAX_SP_OFFSET_X
	if regSize == 4 || regSize == 1 {
		// str w0 and strb instructions have smaller offset range
		maxOffset = MAX_SP_OFFSET_WB
	}

	if offset <= maxOffset {
		// Easy case: offset from base register.
		baseArg := asm.Arg{Reg: baseReg, Offset: offset, Deref: true}
		if regSize == 1 {
			lines = append(lines, asm.Op2("strb", regArg, baseArg))
		} else {
			lines = append(lines, asm.Op2("str", regArg, baseArg))
		}
	} else {
		lines = append(lines, generateLiteralLoad("x9", 8, int64(offset))...)
		lines = append(lines, asm.Op3("add", asm.X9, asm.Reg(baseReg), asm.X9))
		if regSize == 1 {
			lines = append(lines, asm.Op2("strb", regArg, asm.X9.AsDeref()))
		} else {
			lines = append(lines, asm.Op2("str", regArg, asm.X9.AsDeref()))
		}
	}

	return lines
}

func alignSP(offset int) int {
	// SP must always be aligned by 16 bytes on ARM64.
	return util.Align(offset, 16)
}

func getGlobalLabel(name string) string {
	return "_" + strings.TrimPrefix(name, "@")
}
