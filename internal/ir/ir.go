package ir

import (
	"fmt"
	"io"
	"strings"

	"github.com/iley/pirx/internal/util"
)

/*
Intermediate representation for Pirx. This sits between AST and machine code.
The IR is a basic three-address code. Each operation has size associated with it.
That is the size of the result of the operation in bytes.

Here are the currently supported operations:
 * Assign(Target, Value) - assign value to a variable given its name. If a variable name starts with a "@" it's global.
 * AssignByAddr(Target, Value) - assign value to a variable given its address.
 * UnaryOp(Target, Operation, Value) - perform operation Op (e.g. unary minus) and assig result to the Target.
 * BinaryOp(Target, Operation, Left, Right) - performa a binary operation (e.g. add or multiply).
 * Call(Target, Function, Args) - call a function. Includes support for variadic functions.
 * Return(Value) - return from a function.
 * ExternalCall(Target, Function, Args) - call a function using C ABI.
 * ExternalReturn(Value) - return from a function using the C ABI. Mostly neede for main().
 * Jump(Label) - unconditional jump to a label defined via Anchor operation.
 * JumpUnless(Condition, Lable) - jump unless condition is true.
 * Anchor(Lable) - define a label for Jump/JumpUnless.
*/

func IsGlobal(name string) bool {
	return strings.HasPrefix(name, "@")
}

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
	Name     string
	Args     []string
	ArgSizes []int
	Ops      []Op
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
	return fmt.Sprintf("Assign%d(%s, %s)", a.Size, a.Target, a.Value)
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

type AssignGlobal struct {
	Target string
	Value  Arg
	Size   int
}

func (a AssignGlobal) String() string {
	return fmt.Sprintf("Assign%d(%s, %s)", a.Size, a.Target, a.Value)
}

func (a AssignGlobal) GetTarget() string {
	return a.Target
}

func (a AssignGlobal) GetArgs() []Arg {
	return []Arg{a.Value}
}

func (a AssignGlobal) GetSize() int {
	return a.Size
}

type AssignByAddr struct {
	Target Arg
	Value  Arg
	Size   int
}

func (a AssignByAddr) String() string {
	return fmt.Sprintf("AssignByAddr%d(%s, %s)", a.Size, a.Target, a.Value)
}

func (a AssignByAddr) GetTarget() string {
	return ""
}

func (a AssignByAddr) GetArgs() []Arg {
	return []Arg{a.Target, a.Value}
}

func (a AssignByAddr) GetSize() int {
	return a.Size
}

type UnaryOp struct {
	Result    string
	Value     Arg
	Operation string
	Size      int
}

func (o UnaryOp) String() string {
	return fmt.Sprintf("UnaryOp%d(%s = %s %s)", o.Size, o.Result, o.Operation, o.Value)
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
	Result      string
	Left        Arg
	Right       Arg
	Operation   string
	Size        int // result size
	OperandSize int
}

func (o BinaryOp) String() string {
	return fmt.Sprintf("BinaryOp%d(%s = %s/%d %s %s/%d)", o.Size, o.Result, o.Left, o.OperandSize, o.Operation, o.Right, o.OperandSize)
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
	Result   string
	Function string
	Args     []Arg
	ArgSizes []int
	Size     int // Result size.
}

func (c Call) String() string {
	args := []string{}
	for i := 0; i < len(c.Args); i++ {
		args = append(args, fmt.Sprintf("%s/%d", c.Args[i], c.ArgSizes[i]))
	}
	return fmt.Sprintf("Call%d(%s = %s(%s))", c.Size, c.Result, c.Function, strings.Join(args, ", "))
}

func (c Call) GetTarget() string {
	return c.Result
}

func (c Call) GetArgs() []Arg {
	return c.Args
}

func (c Call) GetSize() int {
	return c.Size
}

type ExternalCall struct {
	Result    string
	Function  string
	Args      []Arg
	ArgSizes  []int
	NamedArgs int // How many of the provided arguments correspond to named arguments. Everything else are variadic args.
	Size      int // Result size.
}

func (c ExternalCall) String() string {
	args := []string{}
	for i := 0; i < len(c.Args); i++ {
		args = append(args, fmt.Sprintf("%s/%d", c.Args[i], c.ArgSizes[i]))
	}
	return fmt.Sprintf("ExternalCall%d(%s = %s(%s))", c.Size, c.Result, c.Function, strings.Join(args, ", "))
}

func (c ExternalCall) GetTarget() string {
	return c.Result
}

func (c ExternalCall) GetArgs() []Arg {
	return c.Args
}

func (c ExternalCall) GetSize() int {
	return c.Size
}

type Return struct {
	Value *Arg // nil for bare returns
	Size  int
}

func (r Return) String() string {
	if r.Value != nil {
		return fmt.Sprintf("Return%d(%s)", r.Size, r.Value)
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

type ExternalReturn struct {
	Value *Arg // nil for bare returns
	Size  int
}

func (r ExternalReturn) String() string {
	if r.Value != nil {
		return fmt.Sprintf("ExternalReturn%d(%s)", r.Size, r.Value)
	}
	return "Return()"
}

func (r ExternalReturn) GetTarget() string {
	return ""
}

func (r ExternalReturn) GetArgs() []Arg {
	if r.Value == nil {
		return []Arg{}
	} else {
		return []Arg{*r.Value}
	}
}

func (r ExternalReturn) GetSize() int {
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
	Size      int
	Condition Arg
	Goto      string
}

func (j JumpUnless) String() string {
	return fmt.Sprintf("JumpUnless%d(%s, %s)", j.Size, j.Condition.String(), j.Goto)
}

func (j JumpUnless) GetTarget() string {
	return ""
}

func (j JumpUnless) GetArgs() []Arg {
	return []Arg{j.Condition}
}

func (j JumpUnless) GetSize() int {
	return j.Size
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
	Zero          bool
}

func (a Arg) String() string {
	if a.Variable != "" {
		return a.Variable
	} else if a.LiteralInt != nil {
		return fmt.Sprintf("%d", *a.LiteralInt)
	} else if a.LiteralString != nil {
		return fmt.Sprintf("\"%s\"", util.EscapeString(*a.LiteralString))
	} else if a.Zero {
		return "0"
	}
	panic(fmt.Sprintf("invalid arg value: %#v", a))
}
