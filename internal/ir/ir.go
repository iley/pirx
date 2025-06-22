package ir

import (
	"fmt"
	"io"
	"strings"
)

type BinaryOpType int

const (
	Plus BinaryOpType = iota
	Minus
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

func (b BinaryOpType) String() string {
	switch b {
	case Plus:
		return "+"
	case Minus:
		return "-"
	default:
		panic(fmt.Sprintf("unknown binary operation: %d", int(b)))
	}
}

type Op interface {
	fmt.Stringer
	isOp()
}

type Assign struct {
	Op
	Target string
	Value  Arg
}

func (a Assign) String() string {
	return fmt.Sprintf("Assign(%s, %s)", a.Target, a.Value)
}

func (a Assign) isOp() {}

type BinaryOp struct {
	Op
	Result    string
	Left      Arg
	Right     Arg
	Operation BinaryOpType
}

func (b BinaryOp) String() string {
	return fmt.Sprintf("BinaryOp(%s = %s %s %s)", b.Result, b.Left, b.Operation, b.Right)
}

func (b BinaryOp) isOp() {}

// TODO: How to handle return values?
type Call struct {
	Op
	Result   string
	Function string
	Args     []Arg
}

func (c Call) String() string {
	args := []string{}
	for _, arg := range c.Args {
		args = append(args, arg.String())
	}
	return fmt.Sprintf("Call(%s = %s(%s))", c.Result, c.Function, strings.Join(args, ", "))
}

func (c Call) isOp() {}

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

func (r Return) isOp() {}

type Arg struct {
	Variable   string
	LiteralInt *int
	// TODO: Add more types.
}

func (a Arg) String() string {
	if a.LiteralInt != nil {
		return fmt.Sprintf("%d", *a.LiteralInt)
	}
	if a.Variable != "" {
		return a.Variable
	}
	return "empty"
}
