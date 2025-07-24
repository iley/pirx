package aarch64_darwin

import (
	_ "embed"
	"fmt"
	"io"
	"maps"
	"slices"

	"github.com/iley/pirx/internal/codegen/common"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/types"
	"github.com/iley/pirx/internal/util"
)

const (
	MAX_FUNC_ARGS = 8
	MAX_SP_OFFSET = 504 // maximum offset from SP supproted in load/store instructions.
)

type CodegenContext struct {
	output         io.Writer
	stringLiterals map[string]string

	// Function-specific.
	locals       map[string]int
	frameSize    int
	functionName string
}

func Generate(output io.Writer, irp ir.IrProgram) error {
	// Map from string to a label in the data section.
	stringLiterals := make(map[string]string)
	for i, s := range common.GatherStrings(irp) {
		stringLiterals[s] = fmt.Sprintf(".Lstr%d", i)
	}

	cc := &CodegenContext{
		output:         output,
		stringLiterals: stringLiterals,
	}

	for i, f := range irp.Functions {
		err := generateFunction(cc, f)
		if err != nil {
			return fmt.Errorf("Error when generating code for function %s: %w", f.Name, err)
		}
		if i != len(irp.Functions)-1 {
			// Separate functions with a newline.
			fmt.Fprintf(output, "\n")
		}
	}

	if len(stringLiterals) > 0 {
		fmt.Fprintf(output, "\n// String literals.\n")
		fmt.Fprintf(output, ".data\n")
	}
	for str, label := range stringLiterals {
		fmt.Fprintf(output, "%s: .string \"%s\"\n", label, util.EscapeString(str))
	}

	return nil
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
		return lsizes[a] - lsizes[b]
	})

	// Generate offsets from SP for all locals.
	offset := 0
	for _, lname := range sortedLocals {
		// Align each slot by either 4 bytes (for 32-bit types) or 8 bytes for everything else.
		// This should be a no-op for most slots because of the sorting above.
		var align int
		if lsizes[lname] == 4 {
			align = 4
		} else {
			align = 8
		}

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
	argsBlockSize += types.WORD_SIZE
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
			return nil
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
	if assign.Size%4 != 0 {
		panic(fmt.Errorf("unsupported size %d, expected multiple of 4", assign.Size))
	}

	if assign.Value.Variable != "" {
		generateMemoryCopy(cc, assign.Value, assign.Size, "sp", cc.locals[assign.Target])
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		if assign.Size != 4 && assign.Size != 8 {
			panic(fmt.Errorf("invalid size %d for integer literal %v", assign.Size, assign.Value))
		}
		generateRegisterLoad(cc, 0, assign.Size, assign.Value)
		generateStoreToLocal(cc, 0, assign.Size, assign.Target)
	} else if assign.Value.Zero {
		generateRegisterLoad(cc, 0, 8, assign.Value)
		offset := 0
		for offset < assign.Size {
			if assign.Size-offset >= 8 {
				generateStoreToLocalWithOffset(cc, 0, 8, assign.Target, offset)
				offset += 8
			} else {
				generateStoreToLocalWithOffset(cc, 0, 4, assign.Target, offset)
				offset += 4
			}
		}
	} else {
		panic(fmt.Errorf("Invalid rvalue in assignment: %v", assign.Value))
	}
	return nil
}

func generateAssignmentByAddr(cc *CodegenContext, assign ir.AssignByAddr) error {
	if assign.Size%4 != 0 {
		panic(fmt.Errorf("size %d not supported in generateAssignmentByAddr, expected a multiple of 4", assign.Size))
	}
	if assign.Value.Variable != "" {
		// Variable to variable.
		generateMemoryCopyByAddr(cc, assign.Value, 0, assign.Size, assign.Target)
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		if assign.Size != 4 && assign.Size != 8 {
			panic(fmt.Errorf("invalid size %d for integer literal %v", assign.Size, assign.Value))
		}
		generateRegisterLoad(cc, 0, assign.Size, assign.Value)
		generateStoreByAddr(cc, 0, assign.Size, assign.Target, 0)
	} else if assign.Value.Zero {
		panic("TODO: zero assignment by address not supported yet")
	} else {
		panic(fmt.Errorf("Invalid rvalue in assignment: %v", assign.Value))
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
		generateMemoryCopy(cc, arg, argSize, "sp", -offset)
	}

	// Save x19 to the stack because it currently holds this function's result address.
	offset += types.WORD_SIZE
	savedX19Offset := offset
	generateRegisterStore(cc, 19, types.WORD_SIZE, "sp", -offset)

	// Don't forget about stack alignment.
	offset = alignSP(offset)

	// Store the result address in x19.
	fmt.Fprintf(cc.output, "  add x19, sp, #%d\n", cc.locals[call.Result])

	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", offset)
	fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
	fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", offset)

	// Restore x19.
	fmt.Fprintf(cc.output, "  ldr x19, [sp, #-%d]\n", savedX19Offset)
}

func generateExternalFunctionCall(cc *CodegenContext, call ir.ExternalCall) error {
	if len(call.Args) > MAX_FUNC_ARGS {
		return fmt.Errorf("Too many arguments in a function call. Got %d, only %d are supported", len(call.Args), MAX_FUNC_ARGS)
	}

	for i, arg := range call.Args[0:call.NamedArgs] {
		argSize := call.ArgSizes[i]
		generateRegisterLoad(cc, i, argSize, arg)
	}

	remainingArgs := call.Args[call.NamedArgs:]

	var spShift int

	// Check whether this is a variadic call.
	if len(remainingArgs) > 0 {
		// Darwin deviates from the standard ARM64 ABI for variadic functions.
		// Arguments go on the stack.
		// First, move SP to allocate space for the args. Don't forget to align SP.
		// TODO: Walk the arguments and add their sizes up (including padding).
		spShift = alignSP(types.WORD_SIZE * (len(remainingArgs)))

		// Then generate the stack pushes.
		for i, arg := range remainingArgs {
			argSize := call.ArgSizes[i+call.NamedArgs]
			generateRegisterLoad(cc, 10, argSize, arg)
			// We extend all arguments to 64 bit.
			if argSize == 4 {
				// Sign extend 32-bit values to 64-bit
				fmt.Fprintf(cc.output, "  sxtw x10, w10\n")
			}
			generateRegisterStore(cc, 10, types.WORD_SIZE, "sp", i*types.WORD_SIZE-int(spShift))
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

	// Store the result.
	generateFunctionResultStore(cc, call.Size, call.Result)

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
		generateRegisterLoad(cc, 9, types.WORD_SIZE, op.Value)
		reg := registerByIndex(0, op.Size)
		fmt.Fprintf(cc.output, "  ldr %s, [x9]\n", reg)
		generateStoreToLocal(cc, 0, op.Size, op.Result)
	default:
		panic(fmt.Errorf("unsupported unary operation %s", op.Operation))
	}
	return nil
}

func generateReturn(cc *CodegenContext, ret ir.Return) {
	// Copy the return value into the slot provided by the caller via x19.
	if ret.Value != nil {
		generateMemoryCopy(cc, *ret.Value, ret.Size, "x19", 0)
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
	switch size {
	case 4, 8:
		generateStoreToLocal(cc, 0, size, target)
	case 12, 16:
		generateStoreToLocalWithOffset(cc, 0, 8, target, 0)
		generateStoreToLocalWithOffset(cc, 1, size-8, target, 8)
	default:
		// We shouldn't get any sizes over 16 bytes here because those use indirect return.
		// Sizes that are not multiple of 4 are not supported by the backend at all.
		panic(fmt.Errorf("unsupported result size in function call: %d", size))
	}
}

// generateRegisterLoad generates code for loading a value into a register by its index and size.
// trashes X9.
func generateRegisterLoad(cc *CodegenContext, regIndex, regSize int, arg ir.Arg) {
	generateRegisterLoadWithOffset(cc, regIndex, regSize, arg, 0)
}

func generateRegisterLoadWithOffset(cc *CodegenContext, regIndex, regSize int, arg ir.Arg, offset int) {
	// TODO: Support for non-local variables.
	reg := registerByIndex(regIndex, regSize)

	if arg.Variable != "" {
		fullOffset := int64(cc.locals[arg.Variable]) + int64(offset)
		if fullOffset <= MAX_SP_OFFSET {
			// Easy case: offset from SP.
			fmt.Fprintf(cc.output, "  ldr %s, [sp, #%d]\n", reg, fullOffset)
		} else {
			// Calculate the address in an intermediary register.
			generateLiteralLoad(cc, "x9", fullOffset)
			fmt.Fprintf(cc.output, "  add x9, sp, x9\n")
			fmt.Fprintf(cc.output, "  ldr %s, [x9]\n", reg)
		}
	} else if arg.LiteralInt != nil {
		if offset != 0 {
			panic("cannot load a literal int64 with offset")
		}
		generateLiteralLoad(cc, reg, *arg.LiteralInt)
	} else if arg.LiteralString != nil {
		if offset != 0 {
			panic("cannot load a literal string with offset")
		}
		label := cc.stringLiterals[*arg.LiteralString]
		fmt.Fprintf(cc.output, "  adrp %s, %s@PAGE\n", reg, label)
		fmt.Fprintf(cc.output, "  add %s, %s, %s@PAGEOFF\n", reg, reg, label)
	} else if arg.Zero {
		fmt.Fprintf(cc.output, "  mov %s, #0\n", reg)
	} else {
		panic(fmt.Errorf("invalid arg in code generation: %v", arg))
	}
}
func generateAddressLoad(cc *CodegenContext, regIndex, regSize int, arg ir.Arg) {
	// TODO: Support for non-local variables.
	if arg.Variable == "" {
		panic(fmt.Errorf("can only load address of variables, got %s", arg))
	}
	reg := registerByIndex(regIndex, regSize)
	offset := int64(cc.locals[arg.Variable])
	fmt.Fprintf(cc.output, "  add %s, sp, #%d\n", reg, offset)
}

// generateStoreToLocal generates code for storing a register into a local variable by the register index and size.
// trashes X9.
func generateStoreToLocal(cc *CodegenContext, regIndex, regSize int, target string) {
	generateStoreToLocalWithOffset(cc, regIndex, regSize, target, 0)
}

// generateRegisterStore generates a store to memory location identified by base register and offset.
func generateRegisterStore(cc *CodegenContext, regIndex, regSize int, baseReg string, offset int) {
	reg := registerByIndex(regIndex, regSize)
	if offset <= MAX_SP_OFFSET {
		// Easy case: offset from base register.
		fmt.Fprintf(cc.output, "  str %s, [%s, #%d]\n", reg, baseReg, offset)
	} else {
		generateLiteralLoad(cc, "x9", int64(offset))
		fmt.Fprintf(cc.output, "  add x9, %s, x9\n", baseReg)
		fmt.Fprintf(cc.output, "  str %s, [x9]\n", reg)
	}
}

func generateStoreToLocalWithOffset(cc *CodegenContext, regIndex, regSize int, target string, offset int) {
	fullOffset := cc.locals[target] + offset
	generateRegisterStore(cc, regIndex, regSize, "sp", fullOffset)
}

// generateStoreByAddr generates code for storing a register through a pointer.
// Loads the pointer address into a register and stores the value through it.
func generateStoreByAddr(cc *CodegenContext, regIndex, regSize int, target ir.Arg, offset int) {
	// Load the destination address.
	generateRegisterLoad(cc, 1, types.WORD_SIZE, target)
	addrReg := registerByIndex(1, types.WORD_SIZE)
	srcReg := registerByIndex(regIndex, regSize)
	// Add offset to the address.
	if offset != 0 {
		fmt.Fprintf(cc.output, "  add %s, %s, #%d\n", addrReg, addrReg, offset)
	}
	fmt.Fprintf(cc.output, "  str %s, [%s]\n", srcReg, addrReg)
}

// generateMemoryCopy generates code for copying memory from source to a base register + offset.
// Trashes x9.
func generateMemoryCopy(cc *CodegenContext, source ir.Arg, size int, baseReg string, baseOffset int) {
	tmpRegister := 9
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			generateRegisterLoadWithOffset(cc, tmpRegister, 8, source, offset)
			generateRegisterStore(cc, tmpRegister, 8, baseReg, baseOffset+offset)
			offset += 8
		} else {
			generateRegisterLoadWithOffset(cc, tmpRegister, 4, source, offset)
			generateRegisterStore(cc, tmpRegister, 4, baseReg, baseOffset+offset)
			offset += 4
		}
	}
}

// generateMemoryCopyByAddr generates code for copying memory from source to an address.
func generateMemoryCopyByAddr(cc *CodegenContext, source ir.Arg, regIndex int, size int, target ir.Arg) {
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			generateRegisterLoadWithOffset(cc, regIndex, 8, source, offset)
			generateStoreByAddr(cc, regIndex, 8, target, offset)
			offset += 8
		} else {
			generateRegisterLoadWithOffset(cc, regIndex, 4, source, offset)
			generateStoreByAddr(cc, regIndex, 4, target, offset)
			offset += 4
		}
	}
}

func generateLiteralLoad(cc *CodegenContext, reg string, val int64) {
	fmt.Fprintf(cc.output, "  mov %s, #%d\n", reg, val&0xffff)
	if (val>>16)&0xffff != 0 {
		fmt.Fprintf(cc.output, "  movk %s, #%d, lsl #16\n", reg, (val>>16)&0xffff)
	}
	if (val>>32)&0xffff != 0 {
		fmt.Fprintf(cc.output, "  movk %s, #%d, lsl #32\n", reg, (val>>32)&0xffff)
	}
	if (val>>48)&0xffff != 0 {
		fmt.Fprintf(cc.output, "  movk %s, #%d, lsl #48\n", reg, (val>>48)&0xffff)
	}
}

func registerByIndex(index, size int) string {
	switch size {
	case 4:
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
