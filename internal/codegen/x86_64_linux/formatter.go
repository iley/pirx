package x86_64_linux

import (
	"fmt"
	"io"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/util"
)

func formatProgram(out io.Writer, p asm.Program) {
	for _, fn := range p.Functions {
		formatFunction(out, fn)
	}

	formatStringLiterals(out, p.StringLiterals)
	formatFloatLiterals(out, p.FloatLiterals)
	formatGlobalVariables(out, p.GlobalVariables)
}

func formatFunction(out io.Writer, fn asm.Function) {
	fmt.Fprintf(out, ".text\n")
	fmt.Fprintf(out, ".globl %s\n", fn.Name)
	fmt.Fprintf(out, ".type %s, @function\n", fn.Name)
	fmt.Fprintf(out, "%s:\n", fn.Name)

	for _, line := range fn.Lines {
		formatLine(out, line)
	}
	fmt.Fprintf(out, ".size %s, .-%s\n", fn.Name, fn.Name)
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
		fmt.Fprintf(out, "  # %s", line.Comment)
	}

	fmt.Fprintf(out, "\n")
}

func argToString(arg asm.Arg) string {
	var result string

	// Handle label with register (RIP-relative addressing)
	if arg.Label != "" && arg.Reg != "" {
		if arg.Offset != 0 {
			// label+offset(%rip)
			result = fmt.Sprintf("%s+%d(%%%s)", arg.Label, arg.Offset, arg.Reg)
		} else if arg.Deref {
			result = fmt.Sprintf("%s(%%%s)", arg.Label, arg.Reg)
		} else {
			// For lea instruction: label(%rip)
			result = fmt.Sprintf("%s(%%%s)", arg.Label, arg.Reg)
		}
		return result
	}

	if arg.Deref && arg.Reg == "" {
		panic(fmt.Errorf("invalid arg %#v. dereferencing only supported for registers", arg))
	}

	if arg.Reg != "" {
		if arg.Offset != 0 && arg.Deref {
			result = fmt.Sprintf("%d(%%%s)", arg.Offset, arg.Reg)
		} else if arg.Offset != 0 && !arg.Deref {
			// For lea with offset
			result = fmt.Sprintf("%d(%%%s)", arg.Offset, arg.Reg)
		} else if arg.Deref {
			result = fmt.Sprintf("(%%%s)", arg.Reg)
		} else {
			result = fmt.Sprintf("%%%s", arg.Reg)
		}
	} else if arg.Lsl != 0 {
		panic(fmt.Errorf("lsl not supported on x86_64: %#v", arg))
	} else if arg.Label != "" {
		result = arg.Label
		if arg.Page || arg.PageOffset {
			panic(fmt.Errorf("page/pageoffset not supported on x86_64: %#v", arg))
		}
	} else if arg.Imm != nil {
		result = fmt.Sprintf("$%d", *arg.Imm)
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
		fmt.Fprintf(out, ".align 8\n")
		fmt.Fprintf(out, "%s:\n", sl.Label)
		fmt.Fprintf(out, "  .string \"%s\"\n", util.EscapeString(sl.Text))
	}
}

func formatFloatLiterals(out io.Writer, floatLiterals []asm.FloatLiteral) {
	if len(floatLiterals) == 0 {
		return
	}
	fmt.Fprintf(out, ".section .rodata\n")
	for _, fl := range floatLiterals {
		fmt.Fprintf(out, ".align 8\n")
		fmt.Fprintf(out, "%s:\n", fl.Label)
		fmt.Fprintf(out, "  .double %g\n", fl.Value)
	}
}

func formatGlobalVariables(out io.Writer, globals []asm.GlobalVariable) {
	if len(globals) == 0 {
		return
	}

	fmt.Fprintf(out, ".bss\n")

	for _, g := range globals {
		// On x86_64 Linux, .align directive expects the actual alignment value (in bytes)
		// which is 2^p where p = MinPowerOfTwo(g.Size)
		p2align := util.MinPowerOfTwo(g.Size)
		alignment := 1 << p2align // 2^p
		fmt.Fprintf(out, ".type %s, @object\n", g.Label)
		fmt.Fprintf(out, ".align %d\n", alignment)
		fmt.Fprintf(out, "%s:\n", g.Label)
		fmt.Fprintf(out, ".zero %d\n", g.Size)
	}
}
