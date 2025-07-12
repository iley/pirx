package types

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/util"
)

type StructDescriptor struct {
	Name   string
	Size   int // includes padding
	Fields []StructField
}

type StructField struct {
	Name   string
	Type   ast.Type
	Size   int
	Offset int
}

func MakeStructDescriptor(node *ast.StructDeclaration) (*StructDescriptor, error) {
	desc := &StructDescriptor{
		Name: node.Name,
	}

	offset := 0
	for _, fnode := range node.Fields {
		size, err := GetTypeSize(fnode.Type)
		if err != nil {
			return nil, fmt.Errorf("%d:%d: error in struct declaration: %s", node.Loc.Line, node.Loc.Col, err)
		}
		offset = util.Align(offset+size, size)
		desc.Fields = append(desc.Fields, StructField{
			Name:   fnode.Name,
			Type:   fnode.Type,
			Size:   size,
			Offset: offset,
		})
	}

	desc.Size = offset

	return desc, nil
}
