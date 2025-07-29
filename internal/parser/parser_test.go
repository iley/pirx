package parser

import (
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
)

// Helper function to parse a type string
func parseTypeString(t *testing.T, typeStr string) ast.Type {
	input := strings.NewReader(typeStr)
	lex := lexer.New(input, "test")
	parser := New()
	parser.lexer = lex

	typ, err := parser.parseType()
	if err != nil {
		t.Fatalf("Failed to parse type %q: %v", typeStr, err)
	}
	return typ
}

// Helper function to parse a variable declaration
func parseVarDecl(t *testing.T, code string) *ast.VariableDeclaration {
	input := strings.NewReader(code)
	lex := lexer.New(input, "test")
	parser := New()
	parser.lexer = lex

	// Consume the 'var' keyword first
	varDecl, err := parser.parseVariableDeclaration()
	if err != nil {
		t.Fatalf("Failed to parse variable declaration %q: %v", code, err)
	}
	return varDecl
}

// Helper function to parse a function argument list
func parseFunctionArgs(t *testing.T, code string) []ast.Arg {
	// Wrap in a function signature for parsing
	fullCode := "func test(" + code + ")"
	input := strings.NewReader(fullCode)
	lex := lexer.New(input, "test")
	parser := New()
	parser.lexer = lex

	// Skip 'func' and 'test' tokens
	parser.consume() // func
	parser.consume() // test
	parser.consume() // (

	args, err := parser.parseArguments()
	if err != nil {
		t.Fatalf("Failed to parse function args %q: %v", code, err)
	}
	return args
}

func TestParseSliceTypes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "slice of int",
			input:    "[]int",
			expected: "[]int",
		},
		{
			name:     "slice of string",
			input:    "[]string",
			expected: "[]string",
		},
		{
			name:     "slice of bool",
			input:    "[]bool",
			expected: "[]bool",
		},
		{
			name:     "slice of int64",
			input:    "[]int64",
			expected: "[]int64",
		},
		{
			name:     "slice of int8",
			input:    "[]int8",
			expected: "[]int8",
		},
		{
			name:     "slice of custom type",
			input:    "[]MyStruct",
			expected: "[]MyStruct",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			typ := parseTypeString(t, tt.input)
			if typ.String() != tt.expected {
				t.Errorf("Expected type %q, got %q", tt.expected, typ.String())
			}

			// Verify it's actually a SliceType
			sliceType, ok := typ.(*ast.SliceType)
			if !ok {
				t.Errorf("Expected *ast.SliceType, got %T", typ)
			} else {
				// Check that element type is parsed correctly
				switch tt.input {
				case "[]int":
					if sliceType.ElementType != ast.Int {
						t.Errorf("Expected element type to be ast.Int, got %v", sliceType.ElementType)
					}
				case "[]string":
					if sliceType.ElementType != ast.String {
						t.Errorf("Expected element type to be ast.String, got %v", sliceType.ElementType)
					}
				case "[]bool":
					if sliceType.ElementType != ast.Bool {
						t.Errorf("Expected element type to be ast.Bool, got %v", sliceType.ElementType)
					}
				}
			}
		})
	}
}

func TestParseNestedSliceTypes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "slice of slice of int",
			input:    "[][]int",
			expected: "[][]int",
		},
		{
			name:     "slice of slice of string",
			input:    "[][]string",
			expected: "[][]string",
		},
		{
			name:     "triple nested slice",
			input:    "[][][]int",
			expected: "[][][]int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			typ := parseTypeString(t, tt.input)
			if typ.String() != tt.expected {
				t.Errorf("Expected type %q, got %q", tt.expected, typ.String())
			}

			// Verify the nested structure
			sliceType, ok := typ.(*ast.SliceType)
			if !ok {
				t.Errorf("Expected *ast.SliceType, got %T", typ)
				return
			}

			// For nested slices, the element should also be a slice
			if tt.input == "[][]int" {
				innerSlice, ok := sliceType.ElementType.(*ast.SliceType)
				if !ok {
					t.Errorf("Expected inner element to be *ast.SliceType, got %T", sliceType.ElementType)
				} else if innerSlice.ElementType != ast.Int {
					t.Errorf("Expected innermost element type to be ast.Int, got %v", innerSlice.ElementType)
				}
			}
		})
	}
}

func TestParseSliceOfPointerTypes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "slice of pointer to int",
			input:    "[]*int",
			expected: "[]*int",
		},
		{
			name:     "slice of pointer to string",
			input:    "[]*string",
			expected: "[]*string",
		},
		{
			name:     "slice of pointer to custom type",
			input:    "[]*MyStruct",
			expected: "[]*MyStruct",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			typ := parseTypeString(t, tt.input)
			if typ.String() != tt.expected {
				t.Errorf("Expected type %q, got %q", tt.expected, typ.String())
			}

			// Verify structure: SliceType -> PointerType -> BaseType
			sliceType, ok := typ.(*ast.SliceType)
			if !ok {
				t.Errorf("Expected *ast.SliceType, got %T", typ)
				return
			}

			ptrType, ok := sliceType.ElementType.(*ast.PointerType)
			if !ok {
				t.Errorf("Expected element to be *ast.PointerType, got %T", sliceType.ElementType)
			} else {
				// Check the pointed-to type
				switch tt.input {
				case "[]*int":
					if ptrType.ElementType != ast.Int {
						t.Errorf("Expected pointer element type to be ast.Int, got %v", ptrType.ElementType)
					}
				case "[]*string":
					if ptrType.ElementType != ast.String {
						t.Errorf("Expected pointer element type to be ast.String, got %v", ptrType.ElementType)
					}
				}
			}
		})
	}
}

func TestParsePointerToSliceTypes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "pointer to slice of int",
			input:    "*[]int",
			expected: "*[]int",
		},
		{
			name:     "pointer to slice of string",
			input:    "*[]string",
			expected: "*[]string",
		},
		{
			name:     "pointer to slice of pointer to int",
			input:    "*[]*int",
			expected: "*[]*int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			typ := parseTypeString(t, tt.input)
			if typ.String() != tt.expected {
				t.Errorf("Expected type %q, got %q", tt.expected, typ.String())
			}

			// Verify structure: PointerType -> SliceType
			ptrType, ok := typ.(*ast.PointerType)
			if !ok {
				t.Errorf("Expected *ast.PointerType, got %T", typ)
				return
			}

			sliceType, ok := ptrType.ElementType.(*ast.SliceType)
			if !ok {
				t.Errorf("Expected pointer element to be *ast.SliceType, got %T", ptrType.ElementType)
			} else {
				// Check the slice element type
				switch tt.input {
				case "*[]int":
					if sliceType.ElementType != ast.Int {
						t.Errorf("Expected slice element type to be ast.Int, got %v", sliceType.ElementType)
					}
				case "*[]string":
					if sliceType.ElementType != ast.String {
						t.Errorf("Expected slice element type to be ast.String, got %v", sliceType.ElementType)
					}
				}
			}
		})
	}
}

func TestParseVariableDeclarationWithSliceType(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "var with slice type annotation",
			input:    "var my_slice: []int",
			expected: "[]int",
		},
		{
			name:     "var with nested slice type",
			input:    "var matrix: [][]int",
			expected: "[][]int",
		},
		{
			name:     "var with slice of pointers",
			input:    "var ptr_slice: []*string",
			expected: "[]*string",
		},
		{
			name:     "var with pointer to slice",
			input:    "var slice_ptr: *[]int",
			expected: "*[]int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			varDecl := parseVarDecl(t, tt.input)
			if varDecl.Type == nil {
				t.Errorf("Expected type to be parsed, got nil")
				return
			}
			if varDecl.Type.String() != tt.expected {
				t.Errorf("Expected type %q, got %q", tt.expected, varDecl.Type.String())
			}
		})
	}
}

func TestParseFunctionArgsWithSliceTypes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []string // expected type strings for each argument
	}{
		{
			name:     "single slice argument",
			input:    "arr: []int",
			expected: []string{"[]int"},
		},
		{
			name:     "multiple slice arguments",
			input:    "nums: []int, names: []string",
			expected: []string{"[]int", "[]string"},
		},
		{
			name:     "mixed types with slices",
			input:    "count: int, items: []string, flag: bool",
			expected: []string{"int", "[]string", "bool"},
		},
		{
			name:     "nested slice argument",
			input:    "matrix: [][]int",
			expected: []string{"[][]int"},
		},
		{
			name:     "slice of pointers argument",
			input:    "ptrs: []*MyStruct",
			expected: []string{"[]*MyStruct"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			args := parseFunctionArgs(t, tt.input)
			if len(args) != len(tt.expected) {
				t.Errorf("Expected %d arguments, got %d", len(tt.expected), len(args))
				return
			}

			for i, expectedType := range tt.expected {
				if args[i].Type.String() != expectedType {
					t.Errorf("Argument %d: expected type %q, got %q", i, expectedType, args[i].Type.String())
				}
			}
		})
	}
}

func TestSliceTypeEquality(t *testing.T) {
	// Test that slice types with the same element type are equal
	slice1 := &ast.SliceType{ElementType: ast.Int}
	slice2 := &ast.SliceType{ElementType: ast.Int}
	slice3 := &ast.SliceType{ElementType: ast.String}

	if !slice1.Equals(slice2) {
		t.Error("Expected slice types with same element type to be equal")
	}
	if slice1.Equals(slice3) {
		t.Error("Expected slice types with different element types to be unequal")
	}
	if slice1.Equals(ast.Int) {
		t.Error("Expected slice type to be unequal to base type")
	}
}

func TestParseInvalidSliceTypes(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "missing closing bracket",
			input: "[int",
		},
		{
			name:  "missing element type",
			input: "[]",
		},
		{
			name:  "invalid element type",
			input: "[]123",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			_, err := parser.parseType()
			if err == nil {
				t.Errorf("Expected parsing %q to fail, but it succeeded", tt.input)
			}
		})
	}
}

func TestParseIndexExpressions(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string // Expected AST string representation
	}{
		{
			name:     "simple variable indexing",
			input:    "arr[0]",
			expected: "([] arr 0)",
		},
		{
			name:     "variable with expression index",
			input:    "arr[i + 1]",
			expected: "([] arr (+ i 1))",
		},
		{
			name:     "nested indexing",
			input:    "matrix[0][1]",
			expected: "([] ([] matrix 0) 1)",
		},
		{
			name:     "indexing with function call",
			input:    "get_array()[5]",
			expected: "([] (get_array) 5)",
		},
		{
			name:     "complex nested expression",
			input:    "arr[foo(x, y)]",
			expected: "([] arr (foo x y))",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse expression %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}

			// Verify it's actually an IndexExpression for simple cases
			if tt.name == "simple variable indexing" {
				indexExpr, ok := expr.(*ast.IndexExpression)
				if !ok {
					t.Errorf("Expected *ast.IndexExpression, got %T", expr)
				} else {
					// Check array is a variable reference
					if _, ok := indexExpr.Array.(*ast.VariableReference); !ok {
						t.Errorf("Expected array to be *ast.VariableReference, got %T", indexExpr.Array)
					}
					// Check index is a literal
					if _, ok := indexExpr.Index.(*ast.Literal); !ok {
						t.Errorf("Expected index to be *ast.Literal, got %T", indexExpr.Index)
					}
				}
			}
		})
	}
}

func TestParseIndexingAssignments(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "simple index assignment",
			input:    "arr[0] = 42",
			expected: "(= ([]l arr 0) 42)",
		},
		{
			name:     "index assignment with expression",
			input:    "arr[i + 1] = value * 2",
			expected: "(= ([]l arr (+ i 1)) (* value 2))",
		},
		{
			name:     "nested index assignment",
			input:    "matrix[x][y] = 1",
			expected: "(= ([]l ([]l matrix x) y) 1)",
		},
		{
			name:     "index assignment to field access",
			input:    "obj.arr[5] = foo()",
			expected: "(= ([]l (. obj arr) 5) (foo))",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse assignment %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}

			// Verify it's actually an Assignment
			assignment, ok := expr.(*ast.Assignment)
			if !ok {
				t.Errorf("Expected *ast.Assignment, got %T", expr)
			} else {
				// Verify the target is an IndexLValue
				if _, ok := assignment.Target.(*ast.IndexLValue); !ok {
					t.Errorf("Expected assignment target to be *ast.IndexLValue, got %T", assignment.Target)
				}
			}
		})
	}
}

func TestParseVariableDeclarationWithIndexingInInitializer(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string // expected initializer AST
	}{
		{
			name:     "var with index expression initializer",
			input:    "var x: int = arr[5]",
			expected: "([] arr 5)",
		},
		{
			name:     "var with complex indexing",
			input:    "var item: int = matrix[row][col]",
			expected: "([] ([] matrix row) col)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			varDecl := parseVarDecl(t, tt.input)
			if varDecl.Initializer == nil {
				t.Errorf("Expected initializer to be present, got nil")
				return
			}
			if varDecl.Initializer.String() != tt.expected {
				t.Errorf("Expected initializer %q, got %q", tt.expected, varDecl.Initializer.String())
			}
		})
	}
}

func TestParseFunctionCallWithIndexingArgs(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "function call with indexed argument",
			input:    "printf(\"%d\\n\", arr[5])",
			expected: "(printf \"%d\\n\" ([] arr 5))",
		},
		{
			name:     "function call with multiple indexed arguments",
			input:    "swap(arr[i], arr[j])",
			expected: "(swap ([] arr i) ([] arr j))",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse function call %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}
		})
	}
}

func TestParseMixedFieldAccessAndIndexing(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "field access then indexing",
			input:    "obj.array[5]",
			expected: "([] (. obj array) 5)",
		},
		{
			name:     "indexing then field access",
			input:    "structs[0].field",
			expected: "(. ([] structs 0) field)",
		},
		{
			name:     "complex mixed access",
			input:    "container.items[index].value",
			expected: "(. ([] (. container items) index) value)",
		},
		{
			name:     "assignment to mixed access",
			input:    "obj.array[i] = 42",
			expected: "(= ([]l (. obj array) i) 42)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse mixed access %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}
		})
	}
}

func TestParseInvalidIndexExpressions(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "missing closing bracket",
			input: "arr[5",
		},
		{
			name:  "missing index expression",
			input: "arr[]",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			_, err := parser.parseExpression()
			if err == nil {
				t.Errorf("Expected parsing %q to fail, but it succeeded", tt.input)
			}
		})
	}
}

func TestParseNewExpressionWithSliceCount(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "new slice with integer count",
			input:    "new([]int, 10)",
			expected: "(new []int 10)",
		},
		{
			name:     "new slice with variable count",
			input:    "new([]string, size)",
			expected: "(new []string size)",
		},
		{
			name:     "new slice with expression count",
			input:    "new([]bool, n * 2)",
			expected: "(new []bool (* n 2))",
		},
		{
			name:     "new slice with function call count",
			input:    "new([]int64, get_size())",
			expected: "(new []int64 (get_size))",
		},
		{
			name:     "new nested slice with count",
			input:    "new([][]int, rows)",
			expected: "(new [][]int rows)",
		},
		{
			name:     "new slice of pointers with count",
			input:    "new([]*MyStruct, capacity)",
			expected: "(new []*MyStruct capacity)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse new expression %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}

			// Verify it's actually a NewExpression
			newExpr, ok := expr.(*ast.NewExpression)
			if !ok {
				t.Errorf("Expected *ast.NewExpression, got %T", expr)
			} else {
				// Verify the count is present
				if newExpr.Count == nil {
					t.Errorf("Expected Count to be present, got nil")
				}
				// Verify the type is a slice type
				if _, ok := newExpr.TypeExpr.(*ast.SliceType); !ok {
					t.Errorf("Expected TypeExpr to be *ast.SliceType, got %T", newExpr.TypeExpr)
				}
			}
		})
	}
}

func TestParseNewExpressionWithoutCount(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "new struct without count",
			input:    "new(MyStruct)",
			expected: "(new MyStruct)",
		},
		{
			name:     "new pointer without count",
			input:    "new(*int)",
			expected: "(new *int)",
		},
		{
			name:     "new slice without count",
			input:    "new([]int)",
			expected: "(new []int)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse new expression %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}

			// Verify it's actually a NewExpression
			newExpr, ok := expr.(*ast.NewExpression)
			if !ok {
				t.Errorf("Expected *ast.NewExpression, got %T", expr)
			} else {
				// Verify the count is not present
				if newExpr.Count != nil {
					t.Errorf("Expected Count to be nil, got %v", newExpr.Count)
				}
			}
		})
	}
}

func TestParseVariableDeclarationWithNewSlice(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string // expected initializer AST
	}{
		{
			name:     "var with new slice and count",
			input:    "var s: []int = new([]int, 10)",
			expected: "(new []int 10)",
		},
		{
			name:     "var with new slice and variable count",
			input:    "var buffer: []int8 = new([]int8, buffer_size)",
			expected: "(new []int8 buffer_size)",
		},
		{
			name:     "var with new nested slice",
			input:    "var matrix: [][]int = new([][]int, rows)",
			expected: "(new [][]int rows)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			varDecl := parseVarDecl(t, tt.input)
			if varDecl.Initializer == nil {
				t.Errorf("Expected initializer to be present, got nil")
				return
			}
			if varDecl.Initializer.String() != tt.expected {
				t.Errorf("Expected initializer %q, got %q", tt.expected, varDecl.Initializer.String())
			}

			// Verify the initializer is a NewExpression with count
			newExpr, ok := varDecl.Initializer.(*ast.NewExpression)
			if !ok {
				t.Errorf("Expected initializer to be *ast.NewExpression, got %T", varDecl.Initializer)
			} else if newExpr.Count == nil {
				t.Errorf("Expected NewExpression to have Count, got nil")
			}
		})
	}
}

func TestParseFunctionCallWithNewSliceArgs(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "function call with new slice argument",
			input:    "process(new([]int, size))",
			expected: "(process (new []int size))",
		},
		{
			name:     "function call with multiple new arguments",
			input:    "merge(new([]int, n1), new([]int, n2))",
			expected: "(merge (new []int n1) (new []int n2))",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			expr, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("Failed to parse function call %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}
		})
	}
}

func TestParseInvalidNewExpressionsWithCount(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "missing comma before count",
			input: "new([]int 10)",
		},
		{
			name:  "missing count after comma",
			input: "new([]int,)",
		},
		{
			name:  "missing closing parenthesis",
			input: "new([]int, 10",
		},
		{
			name:  "extra comma",
			input: "new([]int, 10, 20)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input := strings.NewReader(tt.input)
			lex := lexer.New(input, "test")
			parser := New()
			parser.lexer = lex

			_, err := parser.parseExpression()
			if err == nil {
				t.Errorf("Expected parsing %q to fail, but it succeeded", tt.input)
			}
		})
	}
}
