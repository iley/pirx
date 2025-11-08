package ir

import (
	"fmt"

	"github.com/iley/pirx/internal/util"
)

type Arg struct {
	Variable      string
	LiteralInt    *int64
	LiteralFloat  *float64
	LiteralString *string
	Zero          bool
}

func (a Arg) String() string {
	if a.Variable != "" {
		return a.Variable
	} else if a.LiteralInt != nil {
		return fmt.Sprintf("%d", *a.LiteralInt)
	} else if a.LiteralFloat != nil {
		return fmt.Sprintf("%g", *a.LiteralFloat)
	} else if a.LiteralString != nil {
		return fmt.Sprintf("\"%s\"", util.EscapeString(*a.LiteralString))
	} else if a.Zero {
		return "0"
	}
	panic(fmt.Sprintf("invalid arg value: %#v", a))
}

type CallArg struct {
	Arg     Arg
	Size    int
	IsFloat bool
}

func (c CallArg) String() string {
	suffix := ""
	if c.IsFloat {
		suffix = "f"
	}
	return fmt.Sprintf("%s%s/%d", c.Arg, suffix, c.Size)
}
