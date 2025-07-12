package types

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
)

const (
	WORD_SIZE = 8
	BOOL_SIZE = 4
)

func GetTypeSize(typ ast.Type) (int, error) {
	if _, ok := typ.(*ast.PointerType); ok {
		return 8, nil
	} else if typ.Equals(ast.Bool) || typ.Equals(ast.Int) {
		return 4, nil
	} else if typ.Equals(ast.Int64) || typ.Equals(ast.String) {
		return 8, nil
	}
	return 0, fmt.Errorf("unknown type %s", typ)
}

func GetTypeSizeNoError(typ ast.Type) int {
	size, err := GetTypeSize(typ)
	if err != nil {
		panic(err)
	}
	return size
}
