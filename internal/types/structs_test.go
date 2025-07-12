package types

import (
	"testing"

	"github.com/iley/pirx/internal/ast"
)

func TestMakeStructDescriptor_Empty(t *testing.T) {
	structDecl := &ast.StructDeclaration{
		Loc:    ast.Location{Line: 1, Col: 1},
		Name:   "EmptyStruct",
		Fields: []ast.StructField{},
	}

	desc, err := MakeStructDescriptor(structDecl)

	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	if desc.Name != "EmptyStruct" {
		t.Errorf("Expected name 'EmptyStruct', got '%s'", desc.Name)
	}

	if desc.Size != 0 {
		t.Errorf("Expected size 0, got %d", desc.Size)
	}

	if len(desc.Fields) != 0 {
		t.Errorf("Expected 0 fields, got %d", len(desc.Fields))
	}
}

func TestMakeStructDescriptor_SingleField(t *testing.T) {
	structDecl := &ast.StructDeclaration{
		Loc:  ast.Location{Line: 1, Col: 1},
		Name: "SingleFieldStruct",
		Fields: []ast.StructField{
			{
				Loc:  ast.Location{Line: 2, Col: 5},
				Name: "value",
				Type: ast.Int,
			},
		},
	}

	desc, err := MakeStructDescriptor(structDecl)

	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	if desc.Name != "SingleFieldStruct" {
		t.Errorf("Expected name 'SingleFieldStruct', got '%s'", desc.Name)
	}

	if desc.Size != 4 {
		t.Errorf("Expected size 4, got %d", desc.Size)
	}

	if len(desc.Fields) != 1 {
		t.Fatalf("Expected 1 field, got %d", len(desc.Fields))
	}

	field := desc.Fields[0]
	if field.Name != "value" {
		t.Errorf("Expected field name 'value', got '%s'", field.Name)
	}

	if field.Type != ast.Int {
		t.Errorf("Expected field type int, got %v", field.Type)
	}

	if field.Size != 4 {
		t.Errorf("Expected field size 4, got %d", field.Size)
	}

	if field.Offset != 4 {
		t.Errorf("Expected field offset 4, got %d", field.Offset)
	}
}

func TestMakeStructDescriptor_MultipleFields(t *testing.T) {
	structDecl := &ast.StructDeclaration{
		Loc:  ast.Location{Line: 1, Col: 1},
		Name: "MultiFieldStruct",
		Fields: []ast.StructField{
			{
				Loc:  ast.Location{Line: 2, Col: 5},
				Name: "intField",
				Type: ast.Int,
			},
			{
				Loc:  ast.Location{Line: 3, Col: 5},
				Name: "stringField",
				Type: ast.String,
			},
			{
				Loc:  ast.Location{Line: 4, Col: 5},
				Name: "boolField",
				Type: ast.Bool,
			},
		},
	}

	desc, err := MakeStructDescriptor(structDecl)

	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	if desc.Name != "MultiFieldStruct" {
		t.Errorf("Expected name 'MultiFieldStruct', got '%s'", desc.Name)
	}

	// int(4) -> offset 4, string(8) -> offset 16 (aligned), bool(4) -> offset 20
	expectedSize := 20
	if desc.Size != expectedSize {
		t.Errorf("Expected size %d, got %d", expectedSize, desc.Size)
	}

	if len(desc.Fields) != 3 {
		t.Fatalf("Expected 3 fields, got %d", len(desc.Fields))
	}

	// Check int field
	intField := desc.Fields[0]
	if intField.Name != "intField" {
		t.Errorf("Expected field name 'intField', got '%s'", intField.Name)
	}
	if intField.Type != ast.Int {
		t.Errorf("Expected field type int, got %v", intField.Type)
	}
	if intField.Size != 4 {
		t.Errorf("Expected field size 4, got %d", intField.Size)
	}
	if intField.Offset != 4 {
		t.Errorf("Expected field offset 4, got %d", intField.Offset)
	}

	// Check string field
	stringField := desc.Fields[1]
	if stringField.Name != "stringField" {
		t.Errorf("Expected field name 'stringField', got '%s'", stringField.Name)
	}
	if stringField.Type != ast.String {
		t.Errorf("Expected field type string, got %v", stringField.Type)
	}
	if stringField.Size != 8 {
		t.Errorf("Expected field size 8, got %d", stringField.Size)
	}
	if stringField.Offset != 16 {
		t.Errorf("Expected field offset 16, got %d", stringField.Offset)
	}

	// Check bool field
	boolField := desc.Fields[2]
	if boolField.Name != "boolField" {
		t.Errorf("Expected field name 'boolField', got '%s'", boolField.Name)
	}
	if boolField.Type != ast.Bool {
		t.Errorf("Expected field type bool, got %v", boolField.Type)
	}
	if boolField.Size != 4 {
		t.Errorf("Expected field size 4, got %d", boolField.Size)
	}
	if boolField.Offset != 20 {
		t.Errorf("Expected field offset 20, got %d", boolField.Offset)
	}
}

func TestMakeStructDescriptor_PointerFields(t *testing.T) {
	structDecl := &ast.StructDeclaration{
		Loc:  ast.Location{Line: 1, Col: 1},
		Name: "PointerStruct",
		Fields: []ast.StructField{
			{
				Loc:  ast.Location{Line: 2, Col: 5},
				Name: "intPtr",
				Type: ast.NewPointerType(ast.Int),
			},
			{
				Loc:  ast.Location{Line: 3, Col: 5},
				Name: "stringPtr",
				Type: ast.NewPointerType(ast.String),
			},
		},
	}

	desc, err := MakeStructDescriptor(structDecl)

	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	if desc.Name != "PointerStruct" {
		t.Errorf("Expected name 'PointerStruct', got '%s'", desc.Name)
	}

	// Both pointers are 8 bytes, so 8 + 8 = 16
	expectedSize := 16
	if desc.Size != expectedSize {
		t.Errorf("Expected size %d, got %d", expectedSize, desc.Size)
	}

	if len(desc.Fields) != 2 {
		t.Fatalf("Expected 2 fields, got %d", len(desc.Fields))
	}

	// Check first pointer field
	intPtrField := desc.Fields[0]
	if intPtrField.Name != "intPtr" {
		t.Errorf("Expected field name 'intPtr', got '%s'", intPtrField.Name)
	}
	if intPtrField.Size != 8 {
		t.Errorf("Expected field size 8, got %d", intPtrField.Size)
	}
	if intPtrField.Offset != 8 {
		t.Errorf("Expected field offset 8, got %d", intPtrField.Offset)
	}

	// Check second pointer field
	stringPtrField := desc.Fields[1]
	if stringPtrField.Name != "stringPtr" {
		t.Errorf("Expected field name 'stringPtr', got '%s'", stringPtrField.Name)
	}
	if stringPtrField.Size != 8 {
		t.Errorf("Expected field size 8, got %d", stringPtrField.Size)
	}
	if stringPtrField.Offset != 16 {
		t.Errorf("Expected field offset 16, got %d", stringPtrField.Offset)
	}
}

func TestMakeStructDescriptor_MixedTypes(t *testing.T) {
	structDecl := &ast.StructDeclaration{
		Loc:  ast.Location{Line: 1, Col: 1},
		Name: "MixedStruct",
		Fields: []ast.StructField{
			{
				Loc:  ast.Location{Line: 2, Col: 5},
				Name: "boolValue",
				Type: ast.Bool,
			},
			{
				Loc:  ast.Location{Line: 3, Col: 5},
				Name: "int64Value",
				Type: ast.Int64,
			},
			{
				Loc:  ast.Location{Line: 4, Col: 5},
				Name: "intValue",
				Type: ast.Int,
			},
		},
	}

	desc, err := MakeStructDescriptor(structDecl)

	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	if desc.Name != "MixedStruct" {
		t.Errorf("Expected name 'MixedStruct', got '%s'", desc.Name)
	}

	// bool(4) -> offset 4, int64(8) -> offset 16 (aligned), int(4) -> offset 20
	expectedSize := 20
	if desc.Size != expectedSize {
		t.Errorf("Expected size %d, got %d", expectedSize, desc.Size)
	}

	if len(desc.Fields) != 3 {
		t.Fatalf("Expected 3 fields, got %d", len(desc.Fields))
	}

	// Check bool field
	boolField := desc.Fields[0]
	if boolField.Offset != 4 {
		t.Errorf("Expected bool field offset 4, got %d", boolField.Offset)
	}

	// Check int64 field
	int64Field := desc.Fields[1]
	if int64Field.Offset != 16 {
		t.Errorf("Expected int64 field offset 16, got %d", int64Field.Offset)
	}

	// Check int field
	intField := desc.Fields[2]
	if intField.Offset != 20 {
		t.Errorf("Expected int field offset 20, got %d", intField.Offset)
	}
}