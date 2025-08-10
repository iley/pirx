package aarch64_darwin

import (
	_ "embed"
	"fmt"
	"io"
	"maps"
	"slices"
	"strings"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/codegen/common"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/util"
)

const (
	FUNC_CALL_REGISTERS = 8
	MAX_SP_OFFSET       = 504 // maximum offset from SP supproted in load/store instructions.
)

type CodegenContext struct {
	output         io.Writer
	stringLiterals map[string]string

	// Function-specific.
	locals       map[string]int // name -> size
	frameSize    int
	functionName string
}

func Generate(output io.Writer, irp ir.IrProgram) error {
	// Map from string to a label in the data section.
	stringLiterals := make(map[string]string)
	for i, s := range common.GatherStrings(irp) {
		stringLiterals[s] = fmt.Sprintf(".Lstr%d", i)
	}

	globalVariables := common.GatherGlobals(irp)

	cc := &CodegenContext{
		output:         output,
		stringLiterals: stringLiterals,
	}

	for i, f := range irp.Functions {
		err := generateFunction(cc, f)
		if err != nil {
			return fmt.Errorf("error when generating code for function %s: %w", f.Name, err)
		}
		if i != len(irp.Functions)-1 {
			// Separate functions with a newline.
			fmt.Fprintf(output, "\n")
		}
	}

	generateStringLiterals(cc, stringLiterals)
	generateGlobalVariables(cc, globalVariables)

	return nil
}

func generateStringLiterals(cc *CodegenContext, literals map[string]string) {
	if len(literals) == 0 {
		return
	}

	fmt.Fprintf(cc.output, "\n// String literals.\n")
	fmt.Fprintf(cc.output, ".section  __TEXT,__cstring,cstring_literals\n")
	for str, label := range literals {
		fmt.Fprintf(cc.output, "%s: .string \"%s\"\n", label, util.EscapeString(str))
	}
}

func generateGlobalVariables(cc *CodegenContext, globals map[string]int) {
	if len(globals) == 0 {
		return
	}

	sortedGlobals := slices.Collect(maps.Keys(globals))
	slices.Sort(sortedGlobals)

	fmt.Fprintf(cc.output, "// Global variables\n")
	for _, name := range sortedGlobals {
		label := getGlobalLabel(name)
		size := globals[name]
		p2align := util.MinPowerOfTwo(size)
		fmt.Fprintf(cc.output, ".zerofill __DATA,__bss,%s,%d,%d\n", label, size, p2align)
	}
}

func generateFunction(cc *CodegenContext, f ir.IrFunction) error {
	fmt.Fprintf(cc.output, ".globl _%s\n", f.Name)
	fmt.Fprintf(cc.output, ".p2align 2\n")
	fmt.Fprintf(cc.output, "_%s:\n", f.Name)

	// Map from local variable name to its size in bytes.
	lsizes := make(map[string]int)

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range f.Ops {
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

	// Space on stack for the registers wa save (plus padding).
	// * x29 is FP (frame pointer) by MacOS convention.
	// * x30 is LR (link register), it holds the return address.
	// * x19 is used by Pirx for the return value address.
	savedRegisters := 16

	// Calculate the size of the arguments block on the stack.
	argsBlockSize := 0
	for _, argSize := range f.ArgSizes {
		argsBlockSize += argSize
	}
	// Add one word for the saved x19.
	argsBlockSize += ast.WORD_SIZE
	// Include SP padding.
	argsBlockSize = alignSP(argsBlockSize)

	// Function arguments are already on the stack above our frame.
	// Calculate offsets for those.
	argOffset := frameSize + savedRegisters + argsBlockSize
	for i, arg := range f.Args {
		argOffset -= f.ArgSizes[i]
		locals[arg] = argOffset
	}

	// Save the registers.
	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", savedRegisters)
	fmt.Fprintf(cc.output, "  stp x29, x30, [sp]\n")

	// Save frame start in X29.
	fmt.Fprintf(cc.output, "  mov x29, sp\n")
	// Shift SP.
	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", frameSize)

	cc.locals = locals
	cc.frameSize = frameSize
	cc.functionName = f.Name

	for i, op := range f.Ops {
		fmt.Fprintf(cc.output, "// Op %d: %s\n", i, op.String())
		err := generateOp(cc, op)
		if err != nil {
			return err
		}
	}

	fmt.Fprintf(cc.output, ".L%s_exit:\n", f.Name)
	fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", cc.frameSize)
	fmt.Fprintf(cc.output, "  ldp x29, x30, [sp]\n")
	fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", savedRegisters)
	fmt.Fprintf(cc.output, "  ret\n")
	return nil
}

func generateOp(cc *CodegenContext, op ir.Op) error {
	if assign, ok := op.(ir.Assign); ok {
		return generateAssignment(cc, assign)
	} else if assign, ok := op.(ir.AssignByAddr); ok {
		return generateAssignmentByAddr(cc, assign)
	} else if call, ok := op.(ir.Call); ok {
		generateFunctionCall(cc, call)
	} else if call, ok := op.(ir.ExternalCall); ok {
		return generateExternalFunctionCall(cc, call)
	} else if binop, ok := op.(ir.BinaryOp); ok {
		return generateBinaryOp(cc, binop)
	} else if unaryOp, ok := op.(ir.UnaryOp); ok {
		return generateUnaryOp(cc, unaryOp)
	} else if ret, ok := op.(ir.Return); ok {
		generateReturn(cc, ret)
	} else if ret, ok := op.(ir.ExternalReturn); ok {
		generateExternalReturn(cc, ret)
	} else if jump, ok := op.(ir.Jump); ok {
		fmt.Fprintf(cc.output, "  b .L%s_%s\n", cc.functionName, jump.Goto)
	} else if jumpUnless, ok := op.(ir.JumpUnless); ok {
		generateRegisterLoad(cc, 0, jumpUnless.Size, jumpUnless.Condition)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  beq .L%s_%s\n", cc.functionName, jumpUnless.Goto)
	} else if anchor, ok := op.(ir.Anchor); ok {
		fmt.Fprintf(cc.output, ".L%s_%s:\n", cc.functionName, anchor.Label)
	} else {
		panic(fmt.Errorf("unknown op type: %v", op))
	}
	return nil
}

func generateAssignment(cc *CodegenContext, assign ir.Assign) error {
	if assign.Value.Variable != "" {
		if ir.IsGlobal(assign.Target) {
			// Load global's address into x1.
			generateAddressLoad(cc, 1, ast.WORD_SIZE, ir.Arg{Variable: assign.Target})
			generateMemoryCopyToReg(cc, assign.Value, assign.Size, "x1", 0)
		} else {
			generateMemoryCopyToReg(cc, assign.Value, assign.Size, "sp", cc.locals[assign.Target])
		}
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		if ir.IsGlobal(assign.Target) {
			// Load global's address into x1.
			generateAddressLoad(cc, 1, ast.WORD_SIZE, ir.Arg{Variable: assign.Target})
			// Load the value into x0.
			generateRegisterLoad(cc, 0, assign.Size, assign.Value)
			// x0 -> [x1]
			generateRegisterStore(cc, 0, assign.Size, "x1", 0)
		} else {
			generateRegisterLoad(cc, 0, assign.Size, assign.Value)
			generateStoreToLocal(cc, 0, assign.Size, assign.Target)
		}
	} else if assign.Value.Zero {
		if ir.IsGlobal(assign.Target) {
			// Do nothing.
			// This is a hack based on the fact that assignment of Arg{Zero: true} only happens in initialization, and globals are zero-initialized by the linker anyway.
		} else {
			generateRegisterLoad(cc, 0, 8, assign.Value)
			offset := 0
			for offset < assign.Size {
				if assign.Size-offset >= 8 {
					generateStoreToLocalWithOffset(cc, 0, 8, assign.Target, offset)
					offset += 8
				} else if assign.Size-offset >= 4 {
					generateStoreToLocalWithOffset(cc, 0, 4, assign.Target, offset)
					offset += 4
				} else {
					generateStoreToLocalWithOffset(cc, 0, 1, assign.Target, offset)
					offset += 1
				}
			}
		}
	} else {
		panic(fmt.Errorf("invalid rvalue in assignment: %v", assign.Value))
	}
	return nil
}

func generateAssignmentByAddr(cc *CodegenContext, assign ir.AssignByAddr) error {
	if assign.Value.Variable != "" {
		// Variable to variable.
		generateMemoryCopyToReference(cc, assign.Value, assign.Size, assign.Target)
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		generateRegisterLoad(cc, 0, assign.Size, assign.Value)
		generateStoreByAddr(cc, 0, assign.Size, assign.Target, 0)
	} else if assign.Value.Zero {
		panic("TODO: zero assignment by address not supported yet")
	} else {
		panic(fmt.Errorf("invalid rvalue in assignment: %v", assign.Value))
	}
	return nil
}

func generateFunctionCall(cc *CodegenContext, call ir.Call) {
	// For Pirx-native functions we use a simpler calling convention:
	//  * All arguments go on the stack.
	//  * Caller always allocates space for the return value.
	//  * The return value's address is passed in via x19.

	offset := 0
	for i, arg := range call.Args {
		argSize := call.ArgSizes[i]
		offset += argSize
		generateMemoryCopyToReg(cc, arg, argSize, "sp", -offset)
	}

	// Save x19 to the stack because it currently holds this function's result address.
	offset += ast.WORD_SIZE
	savedX19Offset := offset
	generateRegisterStore(cc, 19, ast.WORD_SIZE, "sp", -offset)

	// Don't forget about stack alignment.
	offset = alignSP(offset)

	if call.Result != "" {
		// Store the result address in x19.
		fmt.Fprintf(cc.output, "  add x19, sp, #%d\n", cc.locals[call.Result])
	}

	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", offset)
	fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
	fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", offset)

	// Restore x19.
	fmt.Fprintf(cc.output, "  ldr x19, [sp, #-%d]\n", savedX19Offset)
}

// Generate a call to an external function using C ABI.
// ** WARNING! **
// We don't support the full C ABI here, just the bare minimum that we needed up to this point.
func generateExternalFunctionCall(cc *CodegenContext, call ir.ExternalCall) error {
	remainingArgs := call.Args
	remainingArgSizes := call.ArgSizes

	nextRegister := 0 // X0
	for range call.NamedArgs {
		arg := remainingArgs[0]
		argSize := remainingArgSizes[0]

		// We can split a larger argument into two registers.
		needRegisters := 1
		if argSize > 8 {
			needRegisters = 2
		}

		// Check if we ran out of registers.
		if nextRegister+needRegisters >= FUNC_CALL_REGISTERS {
			break
		}

		switch argSize {
		case 4, 8:
			generateRegisterLoad(cc, nextRegister, argSize, arg)
		case 12:
			generateRegisterLoadWithOffset(cc, nextRegister, 8, arg, 0)
			generateRegisterLoadWithOffset(cc, nextRegister+1, 4, arg, 8)
		case 16:
			generateRegisterLoadWithOffset(cc, nextRegister, 8, arg, 0)
			generateRegisterLoadWithOffset(cc, nextRegister+1, 8, arg, 8)
		default:
			panic(fmt.Errorf("unsupported external function argument size %d", argSize))
		}

		remainingArgs = remainingArgs[1:]
		remainingArgSizes = remainingArgSizes[1:]
		nextRegister += needRegisters
	}

	var spShift int

	// Check whether this is a variadic call.
	if len(remainingArgs) > 0 {
		// Darwin deviates from the standard ARM64 ABI for variadic functions.
		// Arguments go on the stack.
		// First, move SP to allocate space for the args. Don't forget to align SP.
		// TODO: Walk the arguments and add their sizes up (including padding).
		spShift = alignSP(ast.WORD_SIZE * (len(remainingArgs)))

		// Then generate the stack pushes.
		for i, arg := range remainingArgs {
			argSize := call.ArgSizes[i+call.NamedArgs]
			generateRegisterLoad(cc, 10, argSize, arg)
			// We extend all arguments to 64 bit.
			if argSize == 4 {
				// Sign extend 32-bit values to 64-bit
				fmt.Fprintf(cc.output, "  sxtw x10, w10\n")
			}
			generateRegisterStore(cc, 10, ast.WORD_SIZE, "sp", i*ast.WORD_SIZE-int(spShift))
		}
	}

	// Adjust SP just before making the call if we have arguments on the stack.
	if spShift > 0 {
		fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", spShift)
	}

	// Finally, call the function.
	fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)

	// Don't forget to clean up if we put have arguments on the stack.
	if spShift > 0 {
		fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", spShift)
	}

	if call.Result != "" {
		// Store the result.
		generateFunctionResultStore(cc, call.Size, call.Result)
	}

	return nil
}

// TODO: Support unsigned operations.
func generateBinaryOp(cc *CodegenContext, binop ir.BinaryOp) error {
	generateRegisterLoad(cc, 0, binop.OperandSize, binop.Left)
	generateRegisterLoad(cc, 1, binop.OperandSize, binop.Right)

	r0 := registerByIndex(0, binop.OperandSize)
	r1 := registerByIndex(1, binop.OperandSize)
	r2 := registerByIndex(2, binop.OperandSize)

	switch binop.Operation {
	case "+":
		fmt.Fprintf(cc.output, "  add %s, %s, %s\n", r0, r0, r1)
	case "-":
		fmt.Fprintf(cc.output, "  sub %s, %s, %s\n", r0, r0, r1)
	case "*":
		fmt.Fprintf(cc.output, "  mul %s, %s, %s\n", r0, r0, r1)
	case "/":
		fmt.Fprintf(cc.output, "  sdiv %s, %s, %s\n", r0, r0, r1)
	case "%":
		fmt.Fprintf(cc.output, "  sdiv %s, %s, %s\n", r2, r0, r1)
		fmt.Fprintf(cc.output, "  msub %s, %s, %s, %s\n", r0, r2, r1, r0)
	case "==":
		fmt.Fprintf(cc.output, "  cmp %s, %s\n", r0, r1)
		fmt.Fprintf(cc.output, "  cset %s, eq\n", r0)
	case "!=":
		fmt.Fprintf(cc.output, "  cmp %s, %s\n", r0, r1)
		fmt.Fprintf(cc.output, "  cset %s, ne\n", r0)
	case "<":
		fmt.Fprintf(cc.output, "  cmp %s, %s\n", r0, r1)
		fmt.Fprintf(cc.output, "  cset %s, lt\n", r0) // signed <
	case ">":
		fmt.Fprintf(cc.output, "  cmp %s, %s\n", r0, r1)
		fmt.Fprintf(cc.output, "  cset %s, gt\n", r0) // signed >
	case "<=":
		fmt.Fprintf(cc.output, "  cmp %s, %s\n", r0, r1)
		fmt.Fprintf(cc.output, "  cset %s, le\n", r0) // signed <=
	case ">=":
		fmt.Fprintf(cc.output, "  cmp %s, %s\n", r0, r1)
		fmt.Fprintf(cc.output, "  cset %s, ge\n", r0) // signed >=
	case "&&":
		fmt.Fprintf(cc.output, "  cmp %s, #0\n", r0)
		fmt.Fprintf(cc.output, "  ccmp %s, #0, #4, ne\n", r1)
		fmt.Fprintf(cc.output, "  cset %s, ne\n", r0)
	case "||":
		fmt.Fprintf(cc.output, "  cmp %s, #0\n", r0)
		fmt.Fprintf(cc.output, "  ccmp %s, #0, #0, eq\n", r1)
		fmt.Fprintf(cc.output, "  cset %s, ne\n", r0)
	default:
		panic(fmt.Errorf("unsupported binary operation in aarch64-darwing codegen: %v", binop.Operation))
	}

	// Attention! This can be an implicit size cast!
	// A typical example where this happens is "boolean = (int64 == int64)".
	generateStoreToLocal(cc, 0, binop.Size, binop.Result)
	return nil
}

func generateUnaryOp(cc *CodegenContext, op ir.UnaryOp) error {
	switch op.Operation {
	case "!":
		generateRegisterLoad(cc, 0, op.Size, op.Value)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  cset x0, eq\n")
		generateStoreToLocal(cc, 0, op.Size, op.Result)
	case "-":
		generateRegisterLoad(cc, 0, op.Size, op.Value)
		reg := registerByIndex(0, op.Size)
		fmt.Fprintf(cc.output, "  neg %s, %s\n", reg, reg)
		generateStoreToLocal(cc, 0, op.Size, op.Result)
	case "&":
		generateAddressLoad(cc, 0, op.Size, op.Value)
		generateStoreToLocal(cc, 0, op.Size, op.Result)
	case "*":
		generateMemoryCopyFromRef(cc, op.Value, op.Size, op.Result)
	default:
		panic(fmt.Errorf("unsupported unary operation %s", op.Operation))
	}
	return nil
}

func generateReturn(cc *CodegenContext, ret ir.Return) {
	// Copy the return value into the slot provided by the caller via x19.
	if ret.Value != nil {
		generateMemoryCopyToReg(cc, *ret.Value, ret.Size, "x19", 0)
	}

	fmt.Fprintf(cc.output, "  b .L%s_exit\n", cc.functionName)
}

// Return from an externally referenced function i.e. C ABI compatible.
func generateExternalReturn(cc *CodegenContext, ret ir.ExternalReturn) {
	emitB := func() {
		fmt.Fprintf(cc.output, "  b .L%s_exit\n", cc.functionName)
	}

	if ret.Value == nil {
		emitB()
		return
	}

	switch ret.Size {
	case 4:
		generateRegisterLoad(cc, 0, ret.Size, *ret.Value)
		fmt.Fprintf(cc.output, "  sxtw x0, w0\n")
	case 8:
		generateRegisterLoad(cc, 0, ret.Size, *ret.Value)
	case 12, 16:
		// Load first 8 bytes into x0...
		generateRegisterLoadWithOffset(cc, 0, 8, *ret.Value, 0)
		// And the rest into x1/w1.
		generateRegisterLoadWithOffset(cc, 1, ret.Size-8, *ret.Value, 8)
	default:
		// TODO: Support sizes over 16 bytes via indirect return (caller allocates space and passess the address in x19).
		panic(fmt.Errorf("unsupported return value size: %d", ret.Size))
	}

	emitB()
}

func generateFunctionResultStore(cc *CodegenContext, size int, target string) {
	if size > 16 {
		// TODO: Returns over 16 bytes are done via a caller-allocated buffer referenced by x8.
		// Once that is supported on the call side, we can just remove the check here.
		panic("returning values larger than 16 bytes from externa functions is not currently supported")
	}

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
			generateStoreToLocalWithOffset(cc, retReg, 8, target, offset)
			offset += 8
		} else if size-offset >= 4 {
			generateStoreToLocalWithOffset(cc, retReg, 4, target, offset)
			offset += 4
			// Skip the shfit if we're either at the end or at register boundary.
			if size > offset && offset != 8 {
				fmt.Fprintf(cc.output, "  lsr x%d, x%d, #32\n", retReg, retReg)
			}
		} else {
			// Copy one byte at a time as we don't support half-word access currently.
			generateStoreToLocalWithOffset(cc, retReg, 1, target, offset)
			offset += 1
			if size > offset {
				fmt.Fprintf(cc.output, "  lsr x%d, x%d, #8\n", retReg, retReg)
			}
		}
	}
}

// generateRegisterLoad generates code for loading a value into a register by its index and size.
// trashes X9.
func generateRegisterLoad(cc *CodegenContext, regIndex, regSize int, arg ir.Arg) {
	generateRegisterLoadWithOffset(cc, regIndex, regSize, arg, 0)
}

func generateGlobalVariableLoadWithOffset(cc *CodegenContext, reg string, regSize int, variable string, offset int) {
	label := getGlobalLabel(variable)
	fmt.Fprintf(cc.output, "  adrp x9, %s@PAGE\n", label)
	fmt.Fprintf(cc.output, "  add x9, x9, %s@PAGEOFF + %d\n", label, offset)
	if regSize == 1 {
		fmt.Fprintf(cc.output, "  ldrsb %s, [x9]\n", reg)
	} else {
		fmt.Fprintf(cc.output, "  ldr %s, [x9]\n", reg)
	}
}

func generateLocalVariableLoadWithOffset(cc *CodegenContext, reg string, regSize int, variable string, offset int) {
	fullOffset := int64(cc.locals[variable]) + int64(offset)
	if fullOffset <= MAX_SP_OFFSET {
		if regSize == 1 {
			fmt.Fprintf(cc.output, "  ldrsb %s, [sp, #%d]\n", reg, fullOffset)
		} else {
			fmt.Fprintf(cc.output, "  ldr %s, [sp, #%d]\n", reg, fullOffset)
		}
	} else {
		generateLiteralLoad(cc, "x9", 8, fullOffset)
		fmt.Fprintf(cc.output, "  add x9, sp, x9\n")
		if regSize == 1 {
			fmt.Fprintf(cc.output, "  ldrsb %s, [x9]\n", reg)
		} else {
			fmt.Fprintf(cc.output, "  ldr %s, [x9]\n", reg)
		}
	}
}

func generateStringLiteralLoad(cc *CodegenContext, reg string, literal string) {
	label := cc.stringLiterals[literal]
	fmt.Fprintf(cc.output, "  adrp %s, %s@PAGE\n", reg, label)
	fmt.Fprintf(cc.output, "  add %s, %s, %s@PAGEOFF\n", reg, reg, label)
}

func generateRegisterLoadWithOffset(cc *CodegenContext, regIndex, regSize int, arg ir.Arg, offset int) {
	reg := registerByIndex(regIndex, regSize)

	if arg.Variable != "" {
		if ir.IsGlobal(arg.Variable) {
			generateGlobalVariableLoadWithOffset(cc, reg, regSize, arg.Variable, offset)
		} else {
			generateLocalVariableLoadWithOffset(cc, reg, regSize, arg.Variable, offset)
		}
	} else if arg.LiteralInt != nil {
		if offset != 0 {
			panic("cannot load a literal int64 with offset")
		}
		generateLiteralLoad(cc, reg, regSize, *arg.LiteralInt)
	} else if arg.LiteralString != nil {
		if offset != 0 {
			panic("cannot load a literal string with offset")
		}
		if regSize != ast.WORD_SIZE {
			panic(fmt.Errorf("cannot load a string literal into register of size %d", regSize))
		}
		generateStringLiteralLoad(cc, reg, *arg.LiteralString)
	} else if arg.Zero {
		fmt.Fprintf(cc.output, "  mov %s, #0\n", reg)
	} else {
		panic(fmt.Errorf("invalid arg in code generation: %v", arg))
	}
}

func generateAddressLoad(cc *CodegenContext, regIndex, regSize int, arg ir.Arg) {
	if arg.Variable == "" {
		panic(fmt.Errorf("can only load address of variables, got %s", arg))
	}

	// TODO: Get rid of regSize argument.
	if regSize != ast.WORD_SIZE {
		panic(fmt.Errorf("can only load address into a register of size %d", ast.WORD_SIZE))
	}

	reg := registerByIndex(regIndex, regSize)
	if ir.IsGlobal(arg.Variable) {
		label := getGlobalLabel(arg.Variable)
		fmt.Fprintf(cc.output, "  adrp %s, %s@PAGE\n", reg, label)
		fmt.Fprintf(cc.output, "  add %s, %s, %s@PAGEOFF\n", reg, reg, label)
	} else {
		offset := int64(cc.locals[arg.Variable])
		fmt.Fprintf(cc.output, "  add %s, sp, #%d\n", reg, offset)
	}
}

// generateStoreToLocal generates code for storing a register into a local variable by the register index and size.
// trashes X9.
func generateStoreToLocal(cc *CodegenContext, regIndex, regSize int, target string) {
	generateStoreToLocalWithOffset(cc, regIndex, regSize, target, 0)
}

func generateStoreToLocalWithOffset(cc *CodegenContext, regIndex, regSize int, target string, offset int) {
	fullOffset := cc.locals[target] + offset
	generateRegisterStore(cc, regIndex, regSize, "sp", fullOffset)
}

// generateRegisterStore generates a store to memory location identified by base register and offset.
func generateRegisterStore(cc *CodegenContext, regIndex, regSize int, baseReg string, offset int) {
	reg := registerByIndex(regIndex, regSize)
	if offset <= MAX_SP_OFFSET {
		// Easy case: offset from base register.
		if regSize == 1 {
			fmt.Fprintf(cc.output, "  strb %s, [%s, #%d]\n", reg, baseReg, offset)
		} else {
			fmt.Fprintf(cc.output, "  str %s, [%s, #%d]\n", reg, baseReg, offset)
		}
	} else {
		generateLiteralLoad(cc, "x9", 8, int64(offset))
		fmt.Fprintf(cc.output, "  add x9, %s, x9\n", baseReg)
		if regSize == 1 {
			fmt.Fprintf(cc.output, "  strb %s, [x9]\n", reg)
		} else {
			fmt.Fprintf(cc.output, "  str %s, [x9]\n", reg)
		}
	}
}

// generateStoreByAddr generates code for storing a register through a pointer.
// Loads the pointer address into a register and stores the value through it.
func generateStoreByAddr(cc *CodegenContext, regIndex, regSize int, target ir.Arg, offset int) {
	// Load the destination address.
	generateRegisterLoad(cc, 1, ast.WORD_SIZE, target)
	addrReg := registerByIndex(1, ast.WORD_SIZE)
	srcReg := registerByIndex(regIndex, regSize)
	// Add offset to the address.
	if offset != 0 {
		fmt.Fprintf(cc.output, "  add %s, %s, #%d\n", addrReg, addrReg, offset)
	}
	if regSize == 1 {
		fmt.Fprintf(cc.output, "  strb %s, [%s]\n", srcReg, addrReg)
	} else {
		fmt.Fprintf(cc.output, "  str %s, [%s]\n", srcReg, addrReg)
	}
}

// Copy `size` bytes from `source` to the memory location identified by a register and an offset.
// Trashes x0.
func generateMemoryCopyToReg(cc *CodegenContext, source ir.Arg, size int, baseReg string, baseOffset int) {
	tempRegister := 0
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			generateRegisterLoadWithOffset(cc, tempRegister, 8, source, offset)
			generateRegisterStore(cc, tempRegister, 8, baseReg, baseOffset+offset)
			offset += 8
		} else if size-offset >= 4 {
			generateRegisterLoadWithOffset(cc, tempRegister, 4, source, offset)
			generateRegisterStore(cc, tempRegister, 4, baseReg, baseOffset+offset)
			offset += 4
		} else {
			generateRegisterLoadWithOffset(cc, tempRegister, 1, source, offset)
			generateRegisterStore(cc, tempRegister, 1, baseReg, baseOffset+offset)
			offset += 1
		}
	}
}

// Copy `size` bytes from `source` to a memory location based that `targetRef` points to.
// Trashes x0.
func generateMemoryCopyToReference(cc *CodegenContext, source ir.Arg, size int, targetRef ir.Arg) {
	tempRegister := 0
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			generateRegisterLoadWithOffset(cc, tempRegister, 8, source, offset)
			generateStoreByAddr(cc, tempRegister, 8, targetRef, offset)
			offset += 8
		} else if size-offset >= 4 {
			generateRegisterLoadWithOffset(cc, tempRegister, 4, source, offset)
			generateStoreByAddr(cc, tempRegister, 4, targetRef, offset)
			offset += 4
		} else {
			generateRegisterLoadWithOffset(cc, tempRegister, 1, source, offset)
			generateStoreByAddr(cc, tempRegister, 1, targetRef, offset)
			offset += 1
		}
	}
}

// Copy `size` bytes from memory location `sourceRef` points to, to the local variable `target`.
// Trashes x0 and x1.
func generateMemoryCopyFromRef(cc *CodegenContext, sourceRef ir.Arg, size int, target string) {
	// Load the source address to x1.
	sourceAddrReg := 1
	generateRegisterLoad(cc, sourceAddrReg, ast.WORD_SIZE, sourceRef)

	offset := 0
	for offset < size {
		if size-offset >= 8 {
			fmt.Fprintf(cc.output, "  ldr x0, [x1, #%d]\n", offset)
			generateStoreToLocalWithOffset(cc, 0, 8, target, offset)
			offset += 8
		} else if size-offset >= 4 {
			fmt.Fprintf(cc.output, "  ldr w0, [x1, #%d]\n", offset)
			generateStoreToLocalWithOffset(cc, 0, 4, target, offset)
			offset += 4
		} else {
			fmt.Fprintf(cc.output, "  ldrb w0, [x1, #%d]\n", offset)
			generateStoreToLocalWithOffset(cc, 0, 1, target, offset)
			offset += 1
		}
	}
}

func generateLiteralLoad(cc *CodegenContext, reg string, regSize int, val int64) {
	fmt.Fprintf(cc.output, "  mov %s, #%d\n", reg, val&0xffff)
	if (val>>16)&0xffff != 0 {
		fmt.Fprintf(cc.output, "  movk %s, #%d, lsl #16\n", reg, (val>>16)&0xffff)
	}
	if regSize == 8 {
		if (val>>32)&0xffff != 0 {
			fmt.Fprintf(cc.output, "  movk %s, #%d, lsl #32\n", reg, (val>>32)&0xffff)
		}
		if (val>>48)&0xffff != 0 {
			fmt.Fprintf(cc.output, "  movk %s, #%d, lsl #48\n", reg, (val>>48)&0xffff)
		}
	}
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

func alignSP(offset int) int {
	// SP must always be aligned by 16 bytes on ARM64.
	return util.Align(offset, 16)
}

func getGlobalLabel(name string) string {
	return "_" + strings.TrimPrefix(name, "@")
}
