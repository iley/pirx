package aarch64_linux

import (
	"fmt"
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/util"
)

func formatProgram(out io.Writer, p asm.Program) {
	fmt.Fprintf(out, ".arch armv8-a\n")

	for _, fn := range p.Functions {
		formatFunction(out, fn)
	}

	formatStringLiterals(out, p.StringLiterals)
	formatGlobalVariables(out, p.GlobalVariables)
}

func formatFunction(out io.Writer, fn asm.Function) {
	fmt.Fprintf(out, ".text\n")
	fmt.Fprintf(out, ".align 2\n")
	fmt.Fprintf(out, ".global %s\n", fn.Name)
	fmt.Fprintf(out, ".type %s, %%function\n", fn.Name)
	fmt.Fprintf(out, "%s:\n", fn.Name)

	for _, line := range fn.Lines {
		formatLine(out, line)
	}
}

func formatLine(out io.Writer, line asm.Line) {
	if line.Label != "" {
		fmt.Fprintf(out, "%s:", line.Label)
	} else if line.Op != "" {
		fmt.Fprintf(out, "  %s", line.Op)

		if line.Arity >= 1 {
			fmt.Fprintf(out, " %s", argToString(line.Arg1))
		}
		if line.Arity >= 2 {
			fmt.Fprintf(out, ", %s", argToString(line.Arg2))
		}
		if line.Arity >= 3 {
			fmt.Fprintf(out, ", %s", argToString(line.Arg3))
		}
		if line.Arity >= 4 {
			fmt.Fprintf(out, ", %s", argToString(line.Arg4))
		}
	}

	if line.Comment != "" {
		fmt.Fprintf(out, "  // %s", line.Comment)
	}

	fmt.Fprintf(out, "\n")
}

func argToString(arg asm.Arg) string {
	var result string

	if arg.Deref && arg.Reg == "" {
		panic(fmt.Errorf("invalid arg %#v. dereferencing only supported for registers", arg))
	}

	if arg.Reg != "" {
		if arg.Offset != 0 && arg.Deref {
			result = fmt.Sprintf("[%s, %d]", arg.Reg, arg.Offset)
		} else if arg.Deref {
			result = fmt.Sprintf("[%s]", arg.Reg)
		} else {
			result = arg.Reg
		}
	} else if arg.Lsl != 0 {
		result = fmt.Sprintf("lsl %d", arg.Lsl)
	} else if arg.Label != "" {
		result = arg.Label
	} else if arg.Imm != nil {
		result = fmt.Sprintf("%d", *arg.Imm)
	} else {
		panic(fmt.Errorf("invalid arg %#v", arg))
	}

	return result
}

func formatStringLiterals(out io.Writer, stringLiterals []asm.StringLiteral) {
	if len(stringLiterals) == 0 {
		return
	}

	fmt.Fprintf(out, ".section .rodata\n")
	for _, sl := range stringLiterals {
		fmt.Fprintf(out, ".align 3\n")
		fmt.Fprintf(out, "%s:\n", sl.Label)
		fmt.Fprintf(out, "  .string \"%s\"\n", util.EscapeString(sl.Text))
	}
}

func formatGlobalVariables(out io.Writer, globals []asm.GlobalVariable) {
	if len(globals) == 0 {
		return
	}

	fmt.Fprintf(out, ".bss\n")

	for _, g := range globals {
		p2align := util.MinPowerOfTwo(g.Size)
		fmt.Fprintf(out, ".type %s, %%object\n", g.Label)
		fmt.Fprintf(out, ".align %d\n", p2align)
		fmt.Fprintf(out, "%s:\n", g.Label)
		fmt.Fprintf(out, ".zero %d\n", g.Size)
	}
}
