package common

import (
	"maps"
	"slices"

	"github.com/iley/pirx/internal/ir"
)

func GatherStrings(p ir.IrProgram) []string {
	uniqueStrings := map[string]struct{}{}
	for _, f := range p.Functions {
		for _, op := range f.Ops {
			args := op.GetArgs()
			for _, arg := range args {
				if arg.LiteralString != nil {
					uniqueStrings[*arg.LiteralString] = struct{}{}
				}
			}
		}
	}
	result := slices.Collect(maps.Keys(uniqueStrings))
	slices.Sort(result)
	return result
}

func GatherFloats(p ir.IrProgram) []float64 {
	uniqueFloats := map[float64]struct{}{}
	for _, f := range p.Functions {
		for _, op := range f.Ops {
			args := op.GetArgs()
			for _, arg := range args {
				if arg.LiteralFloat != nil {
					uniqueFloats[*arg.LiteralFloat] = struct{}{}
				}
			}
		}
	}
	result := slices.Collect(maps.Keys(uniqueFloats))
	slices.Sort(result)
	return result
}

type NameAndSize struct {
	Name string
	Size int
}

func GatherGlobals(p ir.IrProgram) map[string]int {
	nameToSize := make(map[string]int)
	for _, f := range p.Functions {
		for _, op := range f.Ops {
			target := op.GetTarget()
			if ir.IsGlobal(target) {
				nameToSize[target] = op.GetSize()
			}
		}
	}

	return nameToSize
}
