package aarch64_darwin

import (
	_ "embed"
	"fmt"
	"io"
	"maps"
	"slices"

	"github.com/iley/pirx/internal/codegen/common"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/util"
)

const (
	WORD_SIZE     = 8
	MAX_FUNC_ARGS = 8
)

type CodegenContext struct {
	output         io.Writer
	locals         map[string]int
	frameSize      int
	stringLiterals map[string]string
}

//go:embed prologue.txt
var prologue string

func Generate(output io.Writer, irp ir.IrProgram) error {
	io.WriteString(output, prologue)

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

	// Map from local variable name to its offset on the stack.
	locals := make(map[string]int)

	// Add params to locals.
	for _, param := range f.Params {
		locals[param] = -1
	}

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range f.Ops {
		if target := op.GetTarget(); target != "" {
			if _, seen := locals[target]; seen {
				continue
			}
			// We'll assign the actual offset below.
			locals[target] = -1
		}
	}

	// Allocate space on the stack:
	// * 2 words for FP and LR
	// * Local variables including function arguments.
	reservedWords := 2
	frameSize := (reservedWords + len(locals)) * WORD_SIZE
	// SP must always be aligned by 16 bytes.
	frameSize = util.Align(frameSize, 16)

	// For now we're going to assume that all variables are 64-bit.
	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", frameSize)
	// Store x29 and x30 at the top of the frame.
	fmt.Fprintf(cc.output, "  stp x29, x30, [sp, #%d]\n", frameSize-2*WORD_SIZE)
	fmt.Fprintf(cc.output, "  add x29, sp, #%d\n", frameSize-2*WORD_SIZE)
	fmt.Fprintf(cc.output, "\n")

	// Offset from x29 for locals.
	offset := WORD_SIZE
	for i, param := range f.Params {
		fmt.Fprintf(cc.output, "  str x%d, [x29, #-%d]\n", i, offset)
		locals[param] = offset
		offset += WORD_SIZE
	}

	for _, name := range slices.Collect(maps.Keys(locals)) {
		if locals[name] != -1 {
			// Skip params that were handled above.
			continue
		}
		locals[name] = offset
		offset += WORD_SIZE
	}

	funcCC := cc
	cc.locals = locals
	cc.frameSize = frameSize

	for i, op := range f.Ops {
		fmt.Fprintf(cc.output, ".L%s_op%d: // %s\n", f.Name, i, op.String())
		err := generateOp(funcCC, op)
		if err != nil {
			return nil
		}
	}

	fmt.Fprintf(cc.output, "\n")
	fmt.Fprintf(cc.output, "  ldp x29, x30, [sp, #%d]\n", frameSize-2*WORD_SIZE)
	fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", frameSize)
	fmt.Fprintf(cc.output, "  ret\n")
	return nil
}

func generateOp(cc *CodegenContext, op ir.Op) error {
	if assign, ok := op.(ir.Assign); ok {
		if assign.Value.Variable != "" {
			// Assign variable to variable.
			src := assign.Value.Variable
			dst := assign.Target
			fmt.Fprintf(cc.output, "  ldr x0, [x29, #-%d]\n", cc.locals[src])
			fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[dst])
		} else if assign.Value.LiteralInt != nil {
			// Assign integer constant to variable.
			name := assign.Target
			generateRegisterLoad(cc, "x0", assign.Value)
			fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[name])
		} else {
			panic(fmt.Sprintf("Invalid rvalue in assignment: %v", assign.Value))
		}
	} else if call, ok := op.(ir.Call); ok {
		return generateFunctionCall(cc, call)
	} else if binop, ok := op.(ir.BinaryOp); ok {
		return generateBinaryOp(cc, binop)
	} else if unaryOp, ok := op.(ir.UnaryOp); ok {
		return generateUnaryOp(cc, unaryOp)
	} else if ret, ok := op.(ir.Return); ok {
		if ret.Value != nil {
			generateRegisterLoad(cc, "x0", *ret.Value)
		}
		fmt.Fprintf(cc.output, "  ldp x29, x30, [sp, #%d]\n", cc.frameSize-2*WORD_SIZE)
		fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", cc.frameSize)
		fmt.Fprintf(cc.output, "  ret\n")
	} else {
		panic(fmt.Sprintf("unknown op type: %v", op))
	}
	return nil
}

func generateFunctionCall(cc *CodegenContext, call ir.Call) error {
	if len(call.Args) > MAX_FUNC_ARGS {
		return fmt.Errorf("Too many arguments in a function call. Got %d, only %d are supported", len(call.Args), MAX_FUNC_ARGS)
	}

	if call.Variadic {
		// Darwin deviates from the standard ARM64 ABI for variadic functions.
		// Arguments go on the stack.
		// First, move SP to allocate space for the args. Don't forget to align SP by 16.
		spShift := 0
		if len(call.Args) > 1 {
			spShift = util.Align(len(call.Args)-1, 16)
			fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", spShift)
		}

		// Then generate stack pushes for the arguments except the first one.
		for i, arg := range call.Args[1:] {
			generateRegisterLoad(cc, "x0", arg)
			fmt.Fprintf(cc.output, "  str x0, [sp, #%d]\n", i*WORD_SIZE)
		}

		if len(call.Args) > 0 {
			// Put the first argument into X0.
			generateRegisterLoad(cc, "x0", call.Args[0])
		}

		// Finally, call the function.
		fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
		fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[call.Result])

		// Don't forget to clean up: move SP back.
		if spShift > 0 {
			fmt.Fprintf(cc.output, "  add sp, sp, #%d\n", spShift)
		}
	} else {
		// Non-variadic functions are much easier: put arguments into X0-X7.
		// We're ignoring functions with more than 8 arguments for now.
		// Those are supposed to go onto the stack.
		for i, arg := range call.Args {
			generateRegisterLoad(cc, fmt.Sprintf("x%d", i), arg)
		}
		fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
		fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[call.Result])
	}
	return nil
}

func generateRegisterLoad(cc *CodegenContext, reg string, arg ir.Arg) {
	// TODO: Support for non-local variables.
	if arg.Variable != "" {
		fmt.Fprintf(cc.output, "  ldr %s, [x29, #-%d]\n", reg, cc.locals[arg.Variable])
	} else if arg.LiteralInt != nil {
		val := *arg.LiteralInt
		fmt.Fprintf(cc.output, "  mov %s, %s\n", reg, util.Slice16bits(val, 0))
		if (val<<16)&0xffff != 0 {
			fmt.Fprintf(cc.output, "  movk %s, %s, lsl #16\n", reg, util.Slice16bits(val, 16))
		}
		if (val<<32)&0xffff != 0 {
			fmt.Fprintf(cc.output, "  movk %s, %s, lsl #32\n", reg, util.Slice16bits(val, 32))
		}
		if (val<<48)&0xffff != 0 {
			fmt.Fprintf(cc.output, "  movk %s, %s, lsl #48\n", reg, util.Slice16bits(val, 48))
		}
	} else if arg.LiteralString != nil {
		label := cc.stringLiterals[*arg.LiteralString]
		fmt.Fprintf(cc.output, "  adrp %s, %s@PAGE\n", reg, label)
		fmt.Fprintf(cc.output, "  add %s, %s, %s@PAGEOFF\n", reg, reg, label)
	} else {
		panic(fmt.Sprintf("invalid arg in code generation: %v", arg))
	}
}

// TODO: Support unsigned operations.
// TODO: Test that all operations handle signed/unsigned consistently.
func generateBinaryOp(cc *CodegenContext, binop ir.BinaryOp) error {
	generateRegisterLoad(cc, "x0", binop.Left)
	generateRegisterLoad(cc, "x1", binop.Right)
	switch binop.Operation {
	case "+":
		fmt.Fprintf(cc.output, "  add x0, x0, x1\n")
	case "-":
		fmt.Fprintf(cc.output, "  sub x0, x0, x1\n")
	case "*":
		fmt.Fprintf(cc.output, "  mul x0, x0, x1\n")
	case "/":
		fmt.Fprintf(cc.output, "  sdiv x0, x0, x1\n")
	case "==":
		fmt.Fprintf(cc.output, "  cmp x0, x1\n")
		fmt.Fprintf(cc.output, "  cset x0, eq\n")
	case "!=":
		fmt.Fprintf(cc.output, "  cmp x0, x1\n")
		fmt.Fprintf(cc.output, "  cset x0, ne\n")
	case "<":
		fmt.Fprintf(cc.output, "  cmp x0, x1\n")
		fmt.Fprintf(cc.output, "  cset x0, lt\n") // signed <
	case ">":
		fmt.Fprintf(cc.output, "  cmp x0, x1\n")
		fmt.Fprintf(cc.output, "  cset x0, gt\n") // signed >
	case "<=":
		fmt.Fprintf(cc.output, "  cmp x0, x1\n")
		fmt.Fprintf(cc.output, "  cset x0, le\n") // signed <=
	case ">=":
		fmt.Fprintf(cc.output, "  cmp x0, x1\n")
		fmt.Fprintf(cc.output, "  cset x0, ge\n") // signed >=
	default:
		panic(fmt.Sprintf("unsupported binary operation in aarch64-darwing codegen: %v", binop.Operation))
	}
	fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[binop.Result])
	return nil
}

func generateUnaryOp(cc *CodegenContext, unaryOp ir.UnaryOp) error {
	if unaryOp.Operation == "!" {
		generateRegisterLoad(cc, "x0", unaryOp.Value)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  cset x0, eq\n")
		fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[unaryOp.Result])
		return nil
	}
	panic(fmt.Sprintf("unsupported unary operation %s", unaryOp.Operation))
}
