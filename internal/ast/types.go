package ast

import (
	"fmt"
)

// Type represents a Pirx type
type Type interface {
	fmt.Stringer
	isType()
	// Equals returns true if this type is equal to the other type
	Equals(other Type) bool
}

// BaseType represents primitive types like int, string, bool
type BaseType struct {
	Name string
}

func (b *BaseType) isType() {}

func (b *BaseType) String() string {
	return b.Name
}

func (b *BaseType) Equals(other Type) bool {
	if otherBase, ok := other.(*BaseType); ok {
		return b.Name == otherBase.Name
	}
	return false
}

// PointerType represents a pointer to another type
type PointerType struct {
	ElementType Type
}

func (p *PointerType) isType() {}

func (p *PointerType) String() string {
	return "*" + p.ElementType.String()
}

func (p *PointerType) Equals(other Type) bool {
	if otherPtr, ok := other.(*PointerType); ok {
		return p.ElementType.Equals(otherPtr.ElementType)
	}
	return false
}

func IsPointerType(typ Type) bool {
	_, ok := typ.(*PointerType)
	return ok
}

// Common base types - singleton instances
var (
	Int    = &BaseType{Name: "int"}
	Int8   = &BaseType{Name: "int8"}
	Int64  = &BaseType{Name: "int64"}
	String = &BaseType{Name: "string"}
	Bool   = &BaseType{Name: "bool"}
	Void   = &BaseType{Name: "void"}
	// Not directly accessible to the user.
	VoidPtr = &BaseType{Name: "voidptr"}
	// Not directly accessible to the user.
	NullPtr = &BaseType{Name: "nullptr"}
	// Represents a value that does not have a type either due to an error or because it's not yet known.
	// Not directly accessible to the user.
	Undefined = &BaseType{Name: "undefined"}
)

func IsIntegerType(typ Type) bool {
	return typ == Int || typ == Int8 || typ == Int64
}

// Helper functions for creating types

// NewBaseType creates a base type, returning singleton instances for common types
func NewBaseType(name string) *BaseType {
	switch name {
	case "int":
		return Int
	case "int8":
		return Int8
	case "int64":
		return Int64
	case "string":
		return String
	case "bool":
		return Bool
	case "void":
		return Void
	default:
		return &BaseType{Name: name}
	}
}

// NewPointerType creates a new pointer type
func NewPointerType(elementType Type) *PointerType {
	return &PointerType{ElementType: elementType}
}
