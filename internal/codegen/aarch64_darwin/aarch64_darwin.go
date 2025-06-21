package aarch64_darwin

import (
	_ "embed"
	"fmt"
	"io"
	"maps"
	"slices"

	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/util"
)

const (
	WORD_SIZE     = 8
	MAX_FUNC_ARGS = 8
)

type CodegenContext struct {
	output    io.Writer
	locals    map[string]int
	frameSize int
}

//go:embed prologue.txt
var prologue string

func Generate(output io.Writer, irp ir.IrProgram) error {
	io.WriteString(output, prologue)
	for _, f := range irp.Functions {
		err := generateFunction(output, f)
		if err != nil {
			return fmt.Errorf("Error when generating code for function %s: %w", f.Name, err)
		}
	}
	return nil
}

func generateFunction(output io.Writer, f ir.IrFunction) error {
	fmt.Fprintf(output, ".p2align 2\n")
	fmt.Fprintf(output, ".globl _%s\n", f.Name)
	fmt.Fprintf(output, "_%s:\n", f.Name)

	// Map from local variable name to its offset on the stack.
	locals := make(map[string]int)

	// Add params to locals.
	for _, param := range f.Params {
		locals[param] = -1
	}

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range f.Ops {
		if assign, ok := op.(ir.Assign); ok {
			if _, seen := locals[assign.Target]; seen {
				continue
			}
			// We'll assign the actual offset below.
			locals[assign.Target] = -1
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
	fmt.Fprintf(output, "  sub sp, sp, #%d\n", frameSize)
	// Store x29 and x30 at the top of the frame.
	fmt.Fprintf(output, "  stp x29, x30, [sp, #%d]\n", frameSize-2*WORD_SIZE)
	fmt.Fprintf(output, "  add x29, sp, #%d\n", frameSize-2*WORD_SIZE)
	fmt.Fprintf(output, "\n")

	// Offset from x29 for locals.
	offset := WORD_SIZE
	for i, param := range f.Params {
		fmt.Fprintf(output, "  str x%d, [x29, #-%d]\n", i, offset)
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

	cc := &CodegenContext{
		output:    output,
		locals:    locals,
		frameSize: frameSize,
	}

	for _, op := range f.Ops {
		err := generateOp(cc, op)
		if err != nil {
			return nil
		}
	}

	fmt.Fprintf(output, "\n")
	fmt.Fprintf(output, "  ldp x29, x30, [sp, #%d]\n", frameSize-2*WORD_SIZE)
	fmt.Fprintf(output, "  add sp, sp, #%d\n", frameSize)
	fmt.Fprintf(output, "  ret\n")
	return nil
}

func generateRegisterLoad(cc *CodegenContext, reg string, arg ir.Arg) {
	// TODO: Support for non-local variables.
	if arg.Variable != "" {
		fmt.Fprintf(cc.output, "  ldr %s, [x29, #-%d]\n", reg, cc.locals[arg.Variable])
	} else if arg.ImmediateInt != nil {
		val := *arg.ImmediateInt
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
	}
}

func generateOp(cc *CodegenContext, op ir.Op) error {
	if assign, ok := op.(ir.Assign); ok {
		if assign.Value.Variable != "" {
			// Assign variable to variable.
			src := assign.Value.Variable
			dst := assign.Target
			fmt.Fprintf(cc.output, "  ldr x0, [x29, #-%d]  // %s\n", cc.locals[src], op.String())
			fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[dst])
		} else if assign.Value.ImmediateInt != nil {
			// Assign integer constant to variable.
			name := assign.Target
			generateRegisterLoad(cc, "x0", assign.Value)
			fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[name])
		} else {
			panic(fmt.Sprintf("Invalid rvalue in assignment: %v", assign.Value))
		}
	} else if call, ok := op.(ir.Call); ok {
		if len(call.Args) > MAX_FUNC_ARGS {
			return fmt.Errorf("Too many arguments in a function call. Got %d, only %d are supported", len(call.Args), MAX_FUNC_ARGS)
		}

		for i, arg := range call.Args {
			generateRegisterLoad(cc, fmt.Sprintf("x%d", i), arg)
			fmt.Fprintf(cc.output, "  bl _%s\n", call.Function)
			fmt.Fprintf(cc.output, "  str x0, [x29, #-%d]\n", cc.locals[call.Result])
		}
	} else if _, ok := op.(ir.BinaryOp); ok {
		fmt.Fprintf(cc.output, "  // %s not implemented yet\n", op.String())
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
