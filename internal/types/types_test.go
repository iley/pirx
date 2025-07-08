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

	// Test that primitive types can be compared with ==
	if intType != intType2 {
		t.Errorf("expected int types to be the same instance")
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

func TestSingletonInstances(t *testing.T) {
	// Test that NewBaseType returns singleton instances for common types
	int1 := NewBaseType("int")
	int2 := NewBaseType("int")
	if int1 != int2 {
		t.Errorf("NewBaseType should return the same instance for 'int'")
	}
	if int1 != Int {
		t.Errorf("NewBaseType('int') should return the same instance as Int")
	}

	string1 := NewBaseType("string")
	string2 := NewBaseType("string")
	if string1 != string2 {
		t.Errorf("NewBaseType should return the same instance for 'string'")
	}
	if string1 != String {
		t.Errorf("NewBaseType('string') should return the same instance as String")
	}

	// Test that custom types are different instances
	custom1 := NewBaseType("custom")
	custom2 := NewBaseType("custom")
	if custom1 == custom2 {
		t.Errorf("NewBaseType should return different instances for custom types")
	}

	// Test that primitive types can be compared with ==
	if Int != NewBaseType("int") {
		t.Errorf("Int should be equal to NewBaseType('int') using ==''")
	}
	if String != NewBaseType("string") {
		t.Errorf("String should be equal to NewBaseType('string') using ==''")
	}
	if Bool != NewBaseType("bool") {
		t.Errorf("Bool should be equal to NewBaseType('bool') using ==''")
	}
	if Int64 != NewBaseType("int64") {
		t.Errorf("Int64 should be equal to NewBaseType('int64') using ==''")
	}
}
