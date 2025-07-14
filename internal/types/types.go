package types

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/util"
)

const (
	WORD_SIZE = 8
	BOOL_SIZE = 4
)

type TypeDescriptor interface {
	GetTypeName() string
	GetSize() int
}

type PrimitiveTypeDescriptor struct {
	name string
	size int
}

func (d *PrimitiveTypeDescriptor) GetTypeName() string {
	return d.name
}

func (d *PrimitiveTypeDescriptor) GetSize() int {
	return d.size
}

type StructDescriptor struct {
	Name   string
	Size   int // includes padding
	Fields []StructField
}

func (sd *StructDescriptor) GetTypeName() string {
	return sd.Name
}

func (sd *StructDescriptor) GetSize() int {
	return sd.Size
}

func (sd *StructDescriptor) GetField(name string) ast.Type {
	for _, field := range sd.Fields {
		if field.Name == name {
			return field.Type
		}
	}
	return nil
}

type StructField struct {
	Name   string
	Type   ast.Type
	Size   int
	Offset int
}

func MakeTypeTable(declarations []ast.TypeDeclaration) (*TypeTable, error) {
	tt := NewTypeTable()

	// Put nils into the map as placeholders so that we're aware of all types ahead-of-time in case we use a pointer.
	for _, decl := range declarations {
		tt.ForwardDeclare(decl.GetTypeName())
	}

	// TODO: Rather than rely on declaration order, figure out a way of resolving dependencies between types.
	for _, decl := range declarations {
		if structDecl, ok := decl.(*ast.StructDeclaration); ok {
			structDesc, err := makeStructDescriptor(structDecl, tt)
			if err != nil {
				return nil, err
			}
			tt.Declare(structDesc)
		}
	}

	return tt, nil
}

func makeStructDescriptor(node *ast.StructDeclaration, tt *TypeTable) (*StructDescriptor, error) {
	desc := &StructDescriptor{
		Name: node.Name,
	}

	offset := 0
	for _, fnode := range node.Fields {
		size, err := tt.GetSize(fnode.Type)
		if err != nil {
			return nil, fmt.Errorf("%s: error in struct declaration: %s", node.Loc, err)
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

type TypeTable struct {
	types map[string]TypeDescriptor
}

func NewTypeTable() *TypeTable {
	types := make(map[string]TypeDescriptor)
	declarePrimitiveTypes(types)
	return &TypeTable{
		types: types,
	}
}

func (tt *TypeTable) ForwardDeclare(name string) {
	tt.types[name] = nil
}

func (tt *TypeTable) Declare(td TypeDescriptor) {
	tt.types[td.GetTypeName()] = td
}

func (tt *TypeTable) GetType(name string) TypeDescriptor {
	return tt.types[name]
}

func (tt *TypeTable) GetSize(typ ast.Type) (int, error) {
	if _, ok := typ.(*ast.PointerType); ok {
		// TODO: Validate the type we're pointing to.
		return 8, nil
	} else if baseType, ok := typ.(*ast.BaseType); ok {
		td, ok := tt.types[baseType.Name]
		if !ok {
			return 0, fmt.Errorf("unknown type %s", baseType)
		}
		return td.GetSize(), nil
	}

	return 0, fmt.Errorf("unknown type %s", typ)
}

func (tt *TypeTable) GetSizeNoError(typ ast.Type) int {
	size, err := tt.GetSize(typ)
	if err != nil {
		panic(err)
	}
	return size
}

func (tt *TypeTable) GetStruct(typ ast.Type) (*StructDescriptor, error) {
	pt, ok := typ.(*ast.BaseType)
	if !ok {
		return nil, fmt.Errorf("type %s does not represent a struct", typ)
	}

	typeDesc, exists := tt.types[pt.Name]
	if !exists {
		return nil, fmt.Errorf("type %s does not exist", typ)
	}

	structDesc, ok := typeDesc.(*StructDescriptor)
	if !ok {
		return nil, fmt.Errorf("type %s is not a struct", typ)
	}

	return structDesc, nil
}

func declarePrimitiveTypes(types map[string]TypeDescriptor) {
	types["int"] = &PrimitiveTypeDescriptor{"int", 4}
	types["int64"] = &PrimitiveTypeDescriptor{"int", 8}
	types["bool"] = &PrimitiveTypeDescriptor{"int", 4}
	types["string"] = &PrimitiveTypeDescriptor{"int", 8}
}
