package common

import (
	"maps"
	"math"
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
	// Deduplicate by bit pattern, not by value: 0.0 == -0.0 in Go, but they
	// are distinct constants and must get separate pool entries.
	uniqueFloats := map[uint64]struct{}{}
	for _, f := range p.Functions {
		for _, op := range f.Ops {
			args := op.GetArgs()
			for _, arg := range args {
				if arg.LiteralFloat != nil {
					uniqueFloats[math.Float64bits(*arg.LiteralFloat)] = struct{}{}
				}
			}
		}
	}
	bits := slices.Collect(maps.Keys(uniqueFloats))
	slices.Sort(bits)
	result := make([]float64, len(bits))
	for i, b := range bits {
		result[i] = math.Float64frombits(b)
	}
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
