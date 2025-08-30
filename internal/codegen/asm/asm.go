package asm

var (
	X0  = Arg{Reg: "x0"}
	X1  = Arg{Reg: "x1"}
	X9  = Arg{Reg: "x9"}
	X29 = Arg{Reg: "x29"}
	X30 = Arg{Reg: "x30"}
	W0  = Arg{Reg: "w0"}
	SP  = Arg{Reg: "sp"}
)

type Program struct {
	Functions       []Function
	StringLiterals  []StringLiteral
	GlobalVariables []GlobalVariable
}

type Function struct {
	Name  string
	Lines []Line
}

type Line struct {
	Comment string
	Label   string
	Op      string
	Arg1    Arg
	Arg2    Arg
	Arg3    Arg
	Arg4    Arg
}

type Arg struct {
	Reg    string
	Offset int
	Imm    int
	Label  string
	Lsl    int
	Deref  bool
}

func (a Arg) WithOffset(offset int) Arg {
	result := a
	result.Offset = offset
	return result
}

func (a Arg) WithLSL(shift int) Arg {
	result := a
	result.Lsl = shift
	return result
}

func (a Arg) AsDeref() Arg {
	result := a
	result.Deref = true
	return result
}

type StringLiteral struct {
	Label string
	Text  string
}

type GlobalVariable struct {
	Label string
	Size  int
}

func Imm(value int) Arg {
	return Arg{Imm: value}
}

func DerefWithOffset(arg Arg, offset int) Arg {
	return arg.WithOffset(offset).AsDeref()
}

func Reg(reg string) Arg {
	return Arg{Reg: reg}
}

func Ref(label string) Arg {
	return Arg{Label: label}
}

func LSL(shift int) Arg {
	return Arg{Lsl: shift}
}

func Op0(op string) Line {
	return Line{Op: op}
}

func Op1(op string, arg Arg) Line {
	return Line{Op: op, Arg1: arg}
}

func Op2(op string, arg1, arg2 Arg) Line {
	return Line{Op: op, Arg1: arg1, Arg2: arg2}
}

func Op3(op string, arg1, arg2, arg3 Arg) Line {
	return Line{Op: op, Arg1: arg1, Arg2: arg2, Arg3: arg3}
}

func Op4(op string, arg1, arg2, arg3, arg4 Arg) Line {
	return Line{Op: op, Arg1: arg1, Arg2: arg2, Arg3: arg3, Arg4: arg4}
}

func Comment(text string) Line {
	return Line{Comment: text}
}

func Label(text string) Line {
	return Line{Label: text}
}
