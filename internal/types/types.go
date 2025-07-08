package types

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

// Helper functions for creating common types

// NewBaseType creates a new base type
func NewBaseType(name string) *BaseType {
	return &BaseType{Name: name}
}

// NewPointerType creates a new pointer type
func NewPointerType(elementType Type) *PointerType {
	return &PointerType{ElementType: elementType}
}

// Common base types
var (
	Int    = NewBaseType("int")
	Int64  = NewBaseType("int64")
	String = NewBaseType("string")
	Bool   = NewBaseType("bool")
	Void   = NewBaseType("void")
)
