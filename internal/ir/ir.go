package ir

import (
	"fmt"
	"io"
	"slices"
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
	for i, op := range irf.Ops {
		fmt.Fprintf(writer, "%4d  %s\n", i, op)
	}
}

type Op interface {
	fmt.Stringer
	// Returns the target being modified by the Op or empty string.
	GetTarget() string
	// GetArgs returns all Arg's used in the op.
	// Used e.g. when we need to find all string literals used in a program.
	GetArgs() []Arg
	// Returns size of the operand(s) in bytes.
	GetSize() int
}

type Assign struct {
	Target string
	Value  Arg
	Size   int
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

func (a Assign) GetSize() int {
	return a.Size
}

type UnaryOp struct {
	Result    string
	Value     Arg
	Operation string
	Size      int
}

func (o UnaryOp) String() string {
	return fmt.Sprintf("UnaryOp(%s = %s %s)", o.Result, o.Operation, o.Value)
}
func (o UnaryOp) GetTarget() string {
	return o.Result
}

func (o UnaryOp) GetArgs() []Arg {
	return []Arg{o.Value}
}

func (o UnaryOp) GetSize() int {
	return o.Size
}

type BinaryOp struct {
	Result    string
	Left      Arg
	Right     Arg
	Operation string
	Size      int
}

func (o BinaryOp) String() string {
	return fmt.Sprintf("BinaryOp(%s = %s %s %s)", o.Result, o.Left, o.Operation, o.Right)
}

func (o BinaryOp) GetTarget() string {
	return o.Result
}

func (o BinaryOp) GetArgs() []Arg {
	return []Arg{o.Left, o.Right}
}

func (o BinaryOp) GetSize() int {
	return o.Size
}

type Call struct {
	Result    string
	Function  string
	Args      []Arg
	NamedArgs int // How many of the provided arguments correspond to named arguments. Everything else are treated as variadic args.
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

func (c Call) GetSize() int {
	return 0
}

type Return struct {
	Value *Arg // nil for bare returns
	Size  int
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

func (r Return) GetSize() int {
	return r.Size
}

type Jump struct {
	Goto string
}

func (j Jump) String() string {
	return fmt.Sprintf("Jump(%s)", j.Goto)
}

func (j Jump) GetTarget() string {
	return ""
}

func (j Jump) GetArgs() []Arg {
	return []Arg{}
}

func (j Jump) GetSize() int {
	return 0
}

type JumpUnless struct {
	Condition Arg
	Goto      string
}

func (j JumpUnless) String() string {
	return fmt.Sprintf("JumpUnless(%s, %s)", j.Condition.String(), j.Goto)
}

func (j JumpUnless) GetTarget() string {
	return ""
}

func (j JumpUnless) GetArgs() []Arg {
	return []Arg{j.Condition}
}

func (j JumpUnless) GetSize() int {
	return 0
}

type Anchor struct {
	Label string
}

func (a Anchor) String() string {
	return fmt.Sprintf("Anchor(%s)", a.Label)
}

func (a Anchor) GetTarget() string {
	return ""
}

func (a Anchor) GetArgs() []Arg {
	return []Arg{}
}

func (a Anchor) GetSize() int {
	return 0
}

type Arg struct {
	Variable      string
	LiteralInt    *int64
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

// Optimize runs IR optimizations on the given IR program.
func Optimize(irp IrProgram) IrProgram {
	irp = eliminateIneffectiveAssignments(irp)
	return irp
}

// eliminateIneffectiveAssignments removes ineffective variable assignments.
// An assingment is ineffective if either the variable is never used or it's not used before the next assignment happens.
func eliminateIneffectiveAssignments(irp IrProgram) IrProgram {
	res := IrProgram{
		Functions: slices.Clone(irp.Functions),
	}
	for i := range res.Functions {
		res.Functions[i].Ops = eliminateIneffectiveAssignmentOps(res.Functions[i].Ops)
	}
	return res
}

func eliminateIneffectiveAssignmentOps(ops []Op) []Op {
	res := []Op{}
	for i, op := range ops {
		if !isIneffectiveAssignment(ops, i) {
			res = append(res, op)
		}
	}
	return res
}

func isIneffectiveAssignment(ops []Op, index int) bool {
	first, ok := ops[index].(Assign)
	if !ok {
		return false // not an assignment
	}

	// Find the next assingment.
	// If not found, default to end of the function.
	nextIdx := len(ops)
	for j := index + 1; j < len(ops); j++ {
		if b, ok := ops[j].(Assign); ok && b.Target == first.Target {
			nextIdx = j
			break
		}
	}

	for j := index + 1; j < nextIdx; j++ {
		if _, ok := ops[j].(Jump); ok {
			// It's a jump, all bets are off.
			return false
		}

		if _, ok := ops[j].(JumpUnless); ok {
			// It's a jump, all bets are off.
			return false
		}

		args := ops[j].GetArgs()
		for _, arg := range args {
			if arg.Variable != "" && arg.Variable == first.Target {
				return false
			}
		}
	}

	return true
}
