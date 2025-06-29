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
	// TODO: Generate code for indirect loading to work around the max. offset.
	MAX_SP_OFFSET = 504 // maximum offset from SP supproted in load/store instructions.
)

type CodegenContext struct {
	output         io.Writer
	stringLiterals map[string]string

	// Function-specific.
	locals       map[string]int
	frameSize    int64
	functionName string
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

	// Add args to locals.
	for _, arg := range f.Params {
		locals[arg] = -1
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

	// Space on the stack for local variables including function arguments.
	// For now we're going to assume that all variables are 64-bit.
	// This does not include space for storing X29 and X30.
	// SP must always be aligned by 16 bytes.
	frameSize := util.Align(int64(len(locals)*WORD_SIZE), 16)

	// Save X29 and X30
	fmt.Fprintf(cc.output, "  sub sp, sp, #16\n")
	fmt.Fprintf(cc.output, "  stp x29, x30, [sp]\n")

	// Save frame start in X29.
	fmt.Fprintf(cc.output, "  mov x29, sp\n")
	// Shift SP.
	fmt.Fprintf(cc.output, "  sub sp, sp, #%d\n", frameSize)

	// Offset from SP for locals.
	offset := 0
	for i, arg := range f.Params {
		if offset > MAX_SP_OFFSET {
			panic(fmt.Sprintf("too many locals: maximum offset supported is %d", MAX_SP_OFFSET))
		}
		fmt.Fprintf(cc.output, "  str x%d, [sp, #%d]\n", i, offset)
		locals[arg] = offset
		offset += WORD_SIZE
	}

	for _, name := range slices.Collect(maps.Keys(locals)) {
		if locals[name] != -1 {
			// Skip args that were handled above.
			continue
		}
		locals[name] = offset
		offset += WORD_SIZE
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
		if assign.Value.Variable != "" {
			// Assign value to variable.
			generateRegisterLoad(cc, "x0", assign.Value)
			generateRegisterStore(cc, "x0", assign.Target)
		} else if assign.Value.LiteralInt != nil {
			// Assign integer constant to variable.
			generateRegisterLoad(cc, "x0", assign.Value)
			generateRegisterStore(cc, "x0", assign.Target)
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
		fmt.Fprintf(cc.output, "  b .L%s_exit\n", cc.functionName)
	} else if jump, ok := op.(ir.Jump); ok {
		fmt.Fprintf(cc.output, "  b .L%s\n", jump.Goto)
	} else if jumpUnless, ok := op.(ir.JumpUnless); ok {
		generateRegisterLoad(cc, "x0", jumpUnless.Condition)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  beq .L%s\n", jumpUnless.Goto)
	} else if anchor, ok := op.(ir.Anchor); ok {
		fmt.Fprintf(cc.output, ".L%s:\n", anchor.Label)
	} else {
		panic(fmt.Sprintf("unknown op type: %v", op))
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
		var spShift int64
		if len(call.Args) > 1 {
			spShift = util.Align(int64(len(call.Args)-1), 16)
		}

		// Then generate stack pushes for the arguments except the first one.
		for i, arg := range call.Args[call.NamedArgs:] {
			generateRegisterLoad(cc, "x0", arg)
			fmt.Fprintf(cc.output, "  str x0, [sp, #%d]\n", i*WORD_SIZE-int(spShift))
		}

		for i, arg := range call.Args[0:call.NamedArgs] {
			reg := fmt.Sprintf("x%d", i)
			generateRegisterLoad(cc, reg, arg)
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
		generateRegisterStore(cc, "x0", call.Result)
	} else {
		// Non-variadic functions are much easier: put arguments into X0-X7.
		// We're ignoring functions with more than 8 arguments for now.
		// Those are supposed to go onto the stack.
		for i, arg := range call.Args {
			generateRegisterLoad(cc, fmt.Sprintf("x%d", i), arg)
		}
		fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
		generateRegisterStore(cc, "x0", call.Result)
	}
	return nil
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
	case "%":
		fmt.Fprintf(cc.output, "  sdiv x2, x0, x1\n")
		fmt.Fprintf(cc.output, "  msub x0, x2, x1, x0\n")
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
	case "&&":
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  ccmp x1, #0, #4, ne\n")
		fmt.Fprintf(cc.output, "  cset x0, ne\n")
	case "||":
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  ccmp x1, #0, #0, eq\n")
		fmt.Fprintf(cc.output, "  cset x0, ne\n")
	default:
		panic(fmt.Sprintf("unsupported binary operation in aarch64-darwing codegen: %v", binop.Operation))
	}
	generateRegisterStore(cc, "x0", binop.Result)
	return nil
}

func generateUnaryOp(cc *CodegenContext, unaryOp ir.UnaryOp) error {
	if unaryOp.Operation == "!" {
		generateRegisterLoad(cc, "x0", unaryOp.Value)
		fmt.Fprintf(cc.output, "  cmp x0, #0\n")
		fmt.Fprintf(cc.output, "  cset x0, eq\n")
		generateRegisterStore(cc, "x0", unaryOp.Result)
		return nil
	}
	panic(fmt.Sprintf("unsupported unary operation %s", unaryOp.Operation))
}

// generateRegisterLoad generates code for loading a value into a register.
// trashes X9.
func generateRegisterLoad(cc *CodegenContext, reg string, arg ir.Arg) {
	// TODO: Support for non-local variables.
	if arg.Variable != "" {
		offset := int64(cc.locals[arg.Variable])
		if offset <= MAX_SP_OFFSET {
			// Easy case: offset from SP.
			fmt.Fprintf(cc.output, "  ldr %s, [sp, #%d]\n", reg, offset)
		} else {
			// Calculate the address in an intermediary register.
			generateLiteralLoad(cc, "x9", offset)
			fmt.Fprintf(cc.output, "  add x9, sp, x9\n")
			fmt.Fprintf(cc.output, "  ldr %s, [x9]\n", reg)
		}
	} else if arg.LiteralInt != nil {
		generateLiteralLoad(cc, reg, *arg.LiteralInt)
	} else if arg.LiteralString != nil {
		label := cc.stringLiterals[*arg.LiteralString]
		fmt.Fprintf(cc.output, "  adrp %s, %s@PAGE\n", reg, label)
		fmt.Fprintf(cc.output, "  add %s, %s, %s@PAGEOFF\n", reg, reg, label)
	} else {
		panic(fmt.Sprintf("invalid arg in code generation: %v", arg))
	}
}

// generateRegisterStore generates code for storing a register into a local variable
// trashes X9.
func generateRegisterStore(cc *CodegenContext, reg, target string) {
	offset := int64(cc.locals[target])
	if offset <= MAX_SP_OFFSET {
		// Easy case: offset from SP.
		fmt.Fprintf(cc.output, "  str x0, [sp, #%d]\n", offset)
	} else {
		generateLiteralLoad(cc, "x9", offset)
		fmt.Fprintf(cc.output, "  add x9, sp, x9\n")
		fmt.Fprintf(cc.output, "  str %s, [x9]\n", reg)
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
