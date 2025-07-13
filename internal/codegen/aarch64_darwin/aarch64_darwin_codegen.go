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

	// Add args to locals.
	for i := 0; i < len(f.Args); i++ {
		lsizes[f.Args[i]] = f.ArgSizes[i]
	}

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range f.Ops {
		if target := op.GetTarget(); target != "" {
			if _, seen := lsizes[target]; seen {
				continue
			}
			lsizes[target] = op.GetSize()
		}
	}

	// Space on the stack for local variables including function arguments.
	// For now we're going to assume that all variables are 64-bit.
	// This does not include space for storing X29 and X30.
	// SP must always be aligned by 16 bytes.
	frameSize := 0
	for _, size := range lsizes {
		frameSize += size
	}
	frameSize = util.Align(frameSize, 16)

	// Save X29 and X30
	fmt.Fprintf(cc.output, "  sub sp, sp, #16\n")
	fmt.Fprintf(cc.output, "  stp x29, x30, [sp]\n")

	// Save frame start in X29.
	fmt.Fprintf(cc.output, "  mov x29, sp\n")
	// Shift SP.
	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", frameSize)

	// Map from local variable name to its offset on the stack.
	locals := make(map[string]int)

	// Sort the locals by size to waste less stack space on alignment.
	sortedLocals := slices.Collect(maps.Keys(lsizes))
	slices.SortFunc(sortedLocals, func(a, b string) int {
		return lsizes[a] - lsizes[b]
	})

	// Generate offsets from SP for all locals and function arguments.
	offset := 0
	for _, lname := range sortedLocals {
		if offset > MAX_SP_OFFSET {
			panic(fmt.Errorf("too many locals: maximum offset supported is %d", MAX_SP_OFFSET))
		}
		// Align each slot by its size (e.g. all 64-bit/8-byte value are 8-byte aligned).
		// This should be a no-op for most slots because of the sorting above.
		offset = util.Align(offset, lsizes[lname])
		locals[lname] = offset
		offset += lsizes[lname]
	}

	// For each argument store the value passed to us in the register to the slot on the stack.
	for i, arg := range f.Args {
		argSize := f.ArgSizes[i]
		offset = locals[arg]
		switch argSize {
		case 4:
			fmt.Fprintf(cc.output, "  str w%d, [sp, #%d]\n", i, offset)
		case 8:
			fmt.Fprintf(cc.output, "  str x%d, [sp, #%d]\n", i, offset)
		default:
			panic(fmt.Errorf("invalid argument size when loading function arguments: %d", argSize))
		}
	}

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
	fmt.Fprintf(cc.output, "  add sp, sp, #16\n")
	fmt.Fprintf(cc.output, "  ret\n")
	return nil
}

func generateOp(cc *CodegenContext, op ir.Op) error {
	if assign, ok := op.(ir.Assign); ok {
		return generateAssignment(cc, assign)
	} else if assign, ok := op.(ir.AssignByAddr); ok {
		return generateAssignmentByAddr(cc, assign)
	} else if call, ok := op.(ir.Call); ok {
		return generateFunctionCall(cc, call)
	} else if binop, ok := op.(ir.BinaryOp); ok {
		return generateBinaryOp(cc, binop)
	} else if unaryOp, ok := op.(ir.UnaryOp); ok {
		return generateUnaryOp(cc, unaryOp)
	} else if ret, ok := op.(ir.Return); ok {
		if ret.Value != nil {
			if ret.Size > 8 {
				panic(fmt.Errorf("unsupported return value size: %d", ret.Size))
			}
			generateRegisterLoad(cc, 0, ret.Size, *ret.Value)
			if ret.Size == 4 {
				fmt.Fprintf(cc.output, "  sxtw x0, w0\n")
			}
		}
		fmt.Fprintf(cc.output, "  b .L%s_exit\n", cc.functionName)
	} else if jump, ok := op.(ir.Jump); ok {
		fmt.Fprintf(cc.output, "  b .L%s\n", jump.Goto)
	} else if jumpUnless, ok := op.(ir.JumpUnless); ok {
		generateRegisterLoad(cc, 0, jumpUnless.Size, jumpUnless.Condition)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  beq .L%s\n", jumpUnless.Goto)
	} else if anchor, ok := op.(ir.Anchor); ok {
		fmt.Fprintf(cc.output, ".L%s:\n", anchor.Label)
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
		offset := 0
		// TODO: Consider falling back to memcpy() for really large blocks.
		for offset < assign.Size {
			if assign.Size-offset >= 8 {
				generateRegisterLoadWithOffset(cc, 0, 8, assign.Value, offset)
				generateRegisterStoreWithOffset(cc, 0, 8, assign.Target, offset)
				offset += 8
			} else {
				generateRegisterLoadWithOffset(cc, 0, 4, assign.Value, offset)
				generateRegisterStoreWithOffset(cc, 0, 4, assign.Target, offset)
				offset += 4
			}
		}
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		if assign.Size != 4 {
			panic(fmt.Errorf("invalid size %d for 32-bit literal %v", assign.Size, assign.Value))
		}
		generateRegisterLoad(cc, 0, 4, assign.Value)
		generateRegisterStore(cc, 0, 4, assign.Target)
	} else if assign.Value.LiteralInt64 != nil {
		// Assign integer constant to variable.
		if assign.Size != 8 {
			panic(fmt.Errorf("invalid size %d for 64-bit literal %v", assign.Size, assign.Value))
		}
		generateRegisterLoad(cc, 0, 8, assign.Value)
		generateRegisterStore(cc, 0, 8, assign.Target)
	} else if assign.Value.Zero {
		generateRegisterLoad(cc, 0, 8, assign.Value)
		offset := 0
		for offset < assign.Size {
			if assign.Size-offset >= 8 {
				generateRegisterStoreWithOffset(cc, 0, 8, assign.Target, offset)
				offset += 8
			} else {
				generateRegisterStoreWithOffset(cc, 0, 4, assign.Target, offset)
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
		offset := 0
		for offset < assign.Size {
			if assign.Size-offset >= 8 {
				generateRegisterLoad(cc, 0, 8, assign.Value)
				generateStoreByAddr(cc, 0, 8, assign.Target, offset)
				offset += 8
			} else {
				generateRegisterLoad(cc, 0, 4, assign.Value)
				generateStoreByAddr(cc, 0, 4, assign.Target, offset)
				offset += 4
			}
		}
	} else if assign.Value.LiteralInt != nil {
		// Assign integer constant to variable.
		if assign.Size != 4 {
			panic(fmt.Errorf("invalid size %d for 32-bit literal %v", assign.Size, assign.Value))
		}
		generateRegisterLoad(cc, 0, 4, assign.Value)
		generateStoreByAddr(cc, 0, assign.Size, assign.Target, 0)
	} else if assign.Value.LiteralInt64 != nil {
		// Assign integer constant to variable.
		if assign.Size != 8 {
			panic(fmt.Errorf("invalid size %d for 64-bit literal %v", assign.Size, assign.Value))
		}
		generateRegisterLoad(cc, 0, 8, assign.Value)
		generateStoreByAddr(cc, 0, assign.Size, assign.Target, 0)
	} else if assign.Value.Zero {
		panic("TODO: zero assignment by address not supported yet")
	} else {
		panic(fmt.Errorf("Invalid rvalue in assignment: %v", assign.Value))
	}
	return nil
}

func generateFunctionCall(cc *CodegenContext, call ir.Call) error {
	if len(call.Args) > MAX_FUNC_ARGS {
		return fmt.Errorf("Too many arguments in a function call. Got %d, only %d are supported", len(call.Args), MAX_FUNC_ARGS)
	}

	// Check whether this is a variadic call.
	if len(call.Args) != call.NamedArgs {
		// Darwin deviates from the standard ARM64 ABI for variadic functions.
		// Arguments go on the stack.
		// First, move SP to allocate space for the args. Don't forget to align SP by 16.
		var spShift int
		if len(call.Args) > 1 {
			spShift = util.Align(types.WORD_SIZE*(len(call.Args)-call.NamedArgs), 16)
		}

		// Then generate stack pushes for the arguments except the first one.
		for i, arg := range call.Args[call.NamedArgs:] {
			argSize := call.ArgSizes[i+call.NamedArgs]
			generateRegisterLoad(cc, 0, argSize, arg)
			// We extend all arguments to 64 bit.
			if argSize == 4 {
				// Sign extend 32-bit values to 64-bit
				fmt.Fprintf(cc.output, "  sxtw x0, w0\n")
			}
			fmt.Fprintf(cc.output, "  str x0, [sp, #%d]\n", i*types.WORD_SIZE-int(spShift))
		}

		for i, arg := range call.Args[0:call.NamedArgs] {
			argSize := call.ArgSizes[i]
			generateRegisterLoad(cc, i, argSize, arg)
		}

		if spShift > 0 {
			// Adjust SP just before making the call.
			fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", spShift)
		}

		// Finally, call the function.
		fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)

		// Don't forget to clean up: move SP back.
		if spShift > 0 {
			fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", spShift)
		}

		// Store the result.
		generateRegisterStore(cc, 0, call.Size, call.Result)
	} else {
		// Non-variadic functions are much easier: put arguments into X0-X7.
		// We're ignoring functions with more than 8 arguments for now.
		// Those are supposed to go onto the stack.
		for i, arg := range call.Args {
			argSize := call.ArgSizes[i]
			generateRegisterLoad(cc, i, argSize, arg)
		}
		fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
		generateRegisterStore(cc, 0, call.Size, call.Result)
	}
	return nil
}

// TODO: Support unsigned operations.
func generateBinaryOp(cc *CodegenContext, binop ir.BinaryOp) error {
	generateRegisterLoad(cc, 0, binop.Size, binop.Left)
	generateRegisterLoad(cc, 1, binop.Size, binop.Right)

	r0 := registerByIndex(0, binop.Size)
	r1 := registerByIndex(1, binop.Size)
	r2 := registerByIndex(2, binop.Size)

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
	generateRegisterStore(cc, 0, binop.Size, binop.Result)
	return nil
}

func generateUnaryOp(cc *CodegenContext, op ir.UnaryOp) error {
	switch op.Operation {
	case "!":
		generateRegisterLoad(cc, 0, op.Size, op.Value)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  cset x0, eq\n")
		generateRegisterStore(cc, 0, op.Size, op.Result)
	case "-":
		generateRegisterLoad(cc, 0, op.Size, op.Value)
		reg := registerByIndex(0, op.Size)
		fmt.Fprintf(cc.output, "  neg %s, %s\n", reg, reg)
		generateRegisterStore(cc, 0, op.Size, op.Result)
	case "&":
		generateAddressLoad(cc, 0, op.Size, op.Value)
		generateRegisterStore(cc, 0, op.Size, op.Result)
	default:
		panic(fmt.Errorf("unsupported unary operation %s", op.Operation))
	}
	return nil
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
		// TODO: Generate 32-bit code.
		generateLiteralLoad(cc, reg, int64(*arg.LiteralInt))
	} else if arg.LiteralInt64 != nil {
		generateLiteralLoad(cc, reg, *arg.LiteralInt64)
	} else if arg.LiteralString != nil {
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

// generateRegisterStore generates code for storing a register into a local variable by the register index and size.
// trashes X9.
func generateRegisterStore(cc *CodegenContext, regIndex, regSize int, target string) {
	generateRegisterStoreWithOffset(cc, regIndex, regSize, target, 0)
}

func generateRegisterStoreWithOffset(cc *CodegenContext, regIndex, regSize int, target string, offset int) {
	reg := registerByIndex(regIndex, regSize)
	fullOffset := int64(cc.locals[target]) + int64(offset)
	if fullOffset <= MAX_SP_OFFSET {
		// Easy case: offset from SP.
		fmt.Fprintf(cc.output, "  str %s, [sp, #%d]\n", reg, fullOffset)
	} else {
		generateLiteralLoad(cc, "x9", fullOffset)
		fmt.Fprintf(cc.output, "  add x9, sp, x9\n")
		fmt.Fprintf(cc.output, "  str %s, [x9]\n", reg)
	}
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
