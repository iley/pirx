package types

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
)

const (
	WORD_SIZE = 8
	BOOL_SIZE = 4
)

func GetTypeSize(typ ast.Type) int {
	if _, ok := typ.(*ast.PointerType); ok {
		return 8
	} else if typ.Equals(ast.Bool) || typ.Equals(ast.Int) {
		return 4
	} else if typ.Equals(ast.Int64) || typ.Equals(ast.String) {
		return 8
	}
	panic(fmt.Sprintf("unknown type %s", typ))
}
