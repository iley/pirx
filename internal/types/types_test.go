package types

import (
	"testing"
)

func TestBaseType(t *testing.T) {
	intType := NewBaseType("int")
	stringType := NewBaseType("string")

	// Test String method
	if intType.String() != "int" {
		t.Errorf("expected 'int', got %s", intType.String())
	}

	// Test Equals method
	intType2 := NewBaseType("int")
	if !intType.Equals(intType2) {
		t.Errorf("expected int types to be equal")
	}

	if intType.Equals(stringType) {
		t.Errorf("expected int and string types to be different")
	}
}

func TestPointerType(t *testing.T) {
	intType := NewBaseType("int")
	ptrIntType := NewPointerType(intType)
	ptrPtrIntType := NewPointerType(ptrIntType)

	// Test String method
	if ptrIntType.String() != "*int" {
		t.Errorf("expected '*int', got %s", ptrIntType.String())
	}

	if ptrPtrIntType.String() != "**int" {
		t.Errorf("expected '**int', got %s", ptrPtrIntType.String())
	}

	// Test Equals method
	ptrIntType2 := NewPointerType(NewBaseType("int"))
	if !ptrIntType.Equals(ptrIntType2) {
		t.Errorf("expected pointer int types to be equal")
	}

	ptrStringType := NewPointerType(NewBaseType("string"))
	if ptrIntType.Equals(ptrStringType) {
		t.Errorf("expected pointer int and pointer string to be different")
	}

	if ptrIntType.Equals(intType) {
		t.Errorf("expected pointer int and int to be different")
	}
}

func TestCommonTypes(t *testing.T) {
	// Test predefined common types
	if Int.String() != "int" {
		t.Errorf("expected 'int', got %s", Int.String())
	}

	if String.String() != "string" {
		t.Errorf("expected 'string', got %s", String.String())
	}

	if Bool.String() != "bool" {
		t.Errorf("expected 'bool', got %s", Bool.String())
	}

	if Int64.String() != "int64" {
		t.Errorf("expected 'int64', got %s", Int64.String())
	}
}

func TestComplexTypeEquality(t *testing.T) {
	// Test complex type equality
	intPtr := NewPointerType(Int)
	intPtrPtr := NewPointerType(intPtr)

	// Create equivalent types
	intPtr2 := NewPointerType(NewBaseType("int"))
	intPtrPtr2 := NewPointerType(NewPointerType(NewBaseType("int")))

	if !intPtr.Equals(intPtr2) {
		t.Errorf("equivalent *int types should be equal")
	}

	if !intPtrPtr.Equals(intPtrPtr2) {
		t.Errorf("equivalent **int types should be equal")
	}

	if intPtr.Equals(intPtrPtr) {
		t.Errorf("*int and **int should not be equal")
	}
}
