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
	return slices.Collect(maps.Keys(uniqueStrings))
}
