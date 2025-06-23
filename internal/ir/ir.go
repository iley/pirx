package ir

import (
	"fmt"
	"io"
	"strings"

	"github.com/iley/pirx/internal/util"
)

type IrProgram struct {
	Functions []IrFunction
}

func (irp IrProgram) Print(writer io.Writer) {
	for _, irf := range irp.Functions {
		irf.Print(writer)
		fmt.Fprintf(writer, "\n")
	}
}

type IrFunction struct {
	Name   string
	Params []string // TODO: Types.
	Ops    []Op
}

func (irf IrFunction) Print(writer io.Writer) {
	fmt.Fprintf(writer, "Function %s:\n", irf.Name)
	for _, op := range irf.Ops {
		fmt.Fprintf(writer, "  %s\n", op)
	}
}

type Op interface {
	fmt.Stringer
	// Returns the target being modified by the Op or empty string.
	GetTarget() string
	// GetArgs returns all Arg's used in the op.
	// Used e.g. when we need to find all string literals used in a program.
	GetArgs() []Arg
}

type Assign struct {
	Op
	Target string
	Value  Arg
}

func (a Assign) String() string {
	return fmt.Sprintf("Assign(%s, %s)", a.Target, a.Value)
}

func (a Assign) GetTarget() string {
	return a.Target
}

func (a Assign) GetArgs() []Arg {
	return []Arg{a.Value}
}

type BinaryOp struct {
	Op
	Result    string
	Left      Arg
	Right     Arg
	Operation string
}

func (b BinaryOp) String() string {
	return fmt.Sprintf("BinaryOp(%s = %s %s %s)", b.Result, b.Left, b.Operation, b.Right)
}

func (b BinaryOp) GetTarget() string {
	return b.Result
}

func (b BinaryOp) GetArgs() []Arg {
	return []Arg{b.Left, b.Right}
}

type Call struct {
	Op
	Result   string
	Function string
	Args     []Arg
	Variadic bool
}

func (c Call) String() string {
	args := []string{}
	for _, arg := range c.Args {
		args = append(args, arg.String())
	}
	return fmt.Sprintf("Call(%s = %s(%s))", c.Result, c.Function, strings.Join(args, ", "))
}

func (c Call) GetTarget() string {
	return c.Result
}

func (c Call) GetArgs() []Arg {
	return c.Args
}

type Return struct {
	Op
	Value *Arg // nil for bare returns
}

func (r Return) String() string {
	if r.Value != nil {
		return fmt.Sprintf("Return(%s)", r.Value)
	}
	return "Return()"
}

func (r Return) GetTarget() string {
	return ""
}

func (r Return) GetArgs() []Arg {
	if r.Value == nil {
		return []Arg{}
	} else {
		return []Arg{*r.Value}
	}
}

type Arg struct {
	Variable      string
	LiteralInt    *int
	LiteralString *string
}

func (a Arg) String() string {
	if a.Variable != "" {
		return a.Variable
	} else if a.LiteralInt != nil {
		return fmt.Sprintf("%d", *a.LiteralInt)
	} else if a.LiteralString != nil {
		return fmt.Sprintf("\"%s\"", util.EscapeString(*a.LiteralString))
	}
	return "empty"
}
