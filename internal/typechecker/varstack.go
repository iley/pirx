package typechecker

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
)

type varStack struct {
	frames []varStackFrame
	// Map that tracks how many times a given name has been used in the current function.
	// This is used for renaming variables to make names unque.
	usageCounts map[string]int
}

type varDescriptor struct {
	typ        ast.Type
	uniqueName string
}

type varStackFrame map[string]varDescriptor

func newVarStack() *varStack {
	return &varStack{
		frames:      []varStackFrame{},
		usageCounts: make(map[string]int),
	}
}

func (vs *varStack) startScope() {
	vs.frames = append(vs.frames, make(varStackFrame))
}

func (vs *varStack) endScope() {
	vs.frames = vs.frames[:len(vs.frames)-1]
}

// declare attempts to add a variable to the inner-most frame
// returns whether that was successful
func (vs *varStack) declare(name string, typ ast.Type) bool {
	lastFrame := vs.frames[len(vs.frames)-1]
	if _, exists := lastFrame[name]; exists {
		return false
	}

	vs.usageCounts[name] += 1
	uniqueName := name
	if vs.usageCounts[name] > 1 {
		uniqueName = fmt.Sprintf("%s@%d", name, vs.usageCounts[name]-1)
	}

	lastFrame[name] = varDescriptor{
		typ:        typ,
		uniqueName: uniqueName,
	}

	return true
}

// lookup attempts to lookup a variable's type. Returns nil if variable was not defined.
func (vs varStack) lookup(name string) (ast.Type, string) {
	for i := len(vs.frames) - 1; i >= 0; i-- {
		vd, ok := vs.frames[i][name]
		if ok {
			return vd.typ, vd.uniqueName
		}
	}
	return nil, ""
}
