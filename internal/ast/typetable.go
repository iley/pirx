package ast

import (
	"fmt"

	"github.com/iley/pirx/internal/util"
)

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

func (tt *TypeTable) GetSize(typ Type) (int, error) {
	if _, ok := typ.(*PointerType); ok {
		// TODO: Validate the type we're pointing to.
		return 8, nil
	} else if _, ok := typ.(*SliceType); ok {
		// TODO: Validate the element type.
		return 16, nil
	} else if baseType, ok := typ.(*BaseType); ok {
		td, ok := tt.types[baseType.Name]
		if !ok {
			return 0, fmt.Errorf("unknown type %s", baseType)
		}
		if td == nil {
			return 0, fmt.Errorf("type %s is not fully defined", baseType.Name)
		}
		return td.GetSize(), nil
	}

	return 0, fmt.Errorf("unknown type %s", typ)
}

func (tt *TypeTable) GetSizeNoError(typ Type) int {
	size, err := tt.GetSize(typ)
	if err != nil {
		panic(err)
	}
	return size
}

func (tt *TypeTable) GetStruct(typ Type) (*StructDescriptor, error) {
	pt, ok := typ.(*BaseType)
	if !ok {
		return nil, fmt.Errorf("type %v does not represent a struct", typ)
	}

	typeDesc, exists := tt.types[pt.Name]
	if !exists {
		return nil, fmt.Errorf("type %v does not exist", typ)
	}

	structDesc, ok := typeDesc.(*StructDescriptor)
	if !ok {
		return nil, fmt.Errorf("type %v is not a struct", typ)
	}

	return structDesc, nil
}

func declarePrimitiveTypes(types map[string]TypeDescriptor) {
	types["int"] = &PrimitiveTypeDescriptor{"int", 4}
	types["int8"] = &PrimitiveTypeDescriptor{"int8", 1}
	types["int64"] = &PrimitiveTypeDescriptor{"int64", 8}
	types["bool"] = &PrimitiveTypeDescriptor{"bool", 4}
	types["string"] = &PrimitiveTypeDescriptor{"string", 8}
}

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
	Fields []StructFieldDescriptor
}

func (sd *StructDescriptor) GetTypeName() string {
	return sd.Name
}

func (sd *StructDescriptor) GetSize() int {
	return sd.Size
}

func (sd *StructDescriptor) GetField(name string) *StructFieldDescriptor {
	for _, field := range sd.Fields {
		if field.Name == name {
			return &field
		}
	}
	return nil
}

func (sd *StructDescriptor) GetFieldType(name string) Type {
	field := sd.GetField(name)
	if field == nil {
		return nil
	}
	return field.Type
}

type StructFieldDescriptor struct {
	Name   string
	Type   Type
	Size   int
	Offset int
}

func MakeTypeTable(declarations []TypeDeclaration) (*TypeTable, error) {
	tt := NewTypeTable()

	// Put nils into the map as placeholders so that we're aware of all types ahead-of-time in case we use a pointer.
	for _, decl := range declarations {
		tt.ForwardDeclare(decl.GetTypeName())
	}

	// TODO: Rather than rely on declaration order, figure out a way of resolving dependencies between types.
	for _, decl := range declarations {
		if structDecl, ok := decl.(*StructDeclaration); ok {
			structDesc, err := makeStructDescriptor(structDecl, tt)
			if err != nil {
				return nil, err
			}
			tt.Declare(structDesc)
		}
	}

	return tt, nil
}

func makeStructDescriptor(node *StructDeclaration, tt *TypeTable) (*StructDescriptor, error) {
	desc := &StructDescriptor{
		Name: node.Name,
	}

	offset := 0
	for _, fnode := range node.Fields {
		size, err := tt.GetSize(fnode.Type)
		if err != nil {
			return nil, fmt.Errorf("%s: error in struct declaration: %s", node.Loc, err)
		}
		desc.Fields = append(desc.Fields, StructFieldDescriptor{
			Name:   fnode.Name,
			Type:   fnode.Type,
			Size:   size,
			Offset: util.Align(offset, size),
		})
		offset = util.Align(offset, size) + size
	}

	desc.Size = offset

	return desc, nil
}
