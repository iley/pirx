package parser

import (
	"reflect"
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/types"
)

// compareASTIgnoreLocation recursively compares AST nodes while ignoring Location fields
func compareASTIgnoreLocation(a, b any) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}

	va := reflect.ValueOf(a)
	vb := reflect.ValueOf(b)

	if !va.IsValid() && !vb.IsValid() {
		return true
	}
	if !va.IsValid() || !vb.IsValid() {
		return false
	}

	if va.Type() != vb.Type() {
		return false
	}

	switch va.Kind() {
	case reflect.Ptr:
		if va.IsNil() && vb.IsNil() {
			return true
		}
		if va.IsNil() || vb.IsNil() {
			return false
		}
		return compareASTIgnoreLocation(va.Elem().Interface(), vb.Elem().Interface())

	case reflect.Struct:
		for i := 0; i < va.NumField(); i++ {
			field := va.Type().Field(i)
			// Skip Location fields
			if field.Name == "Loc" || field.Type.Name() == "Location" {
				continue
			}
			if !compareASTIgnoreLocation(va.Field(i).Interface(), vb.Field(i).Interface()) {
				return false
			}
		}
		return true

	case reflect.Slice:
		if va.Len() != vb.Len() {
			return false
		}
		for i := 0; i < va.Len(); i++ {
			if !compareASTIgnoreLocation(va.Index(i).Interface(), vb.Index(i).Interface()) {
				return false
			}
		}
		return true

	default:
		return reflect.DeepEqual(a, b)
	}
}

func TestParseProgram(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "trivial program",
			src:  `func main() {}`,
			expected: &ast.Program{
				Loc: ast.Location{Line: 1, Col: 1},
				Functions: []ast.Function{
					{
						Loc:  ast.Location{Line: 1, Col: 1},
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{Loc: ast.Location{Line: 1, Col: 14}, Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with var declaration",
			src:  `func main() { var x: int; }`,
			expected: &ast.Program{
				Loc: ast.Location{Line: 1, Col: 1},
				Functions: []ast.Function{
					{
						Loc:  ast.Location{Line: 1, Col: 1},
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Loc: ast.Location{Line: 1, Col: 15},
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Loc: ast.Location{Line: 1, Col: 15}, Name: "x", Type: types.Int},
							},
						},
					},
				},
			},
		},
		{
			name: "function with arguments",
			src:  `func add(a: int, b: int) {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "add",
						Args: []ast.Arg{
							{Name: "a", Type: types.Int},
							{Name: "b", Type: types.Int},
						},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with expression statements",
			src:  `func main() { foo(1, "two"); }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ExpressionStatement{
									Expression: &ast.FunctionCall{
										FunctionName: "foo",
										Args: []ast.Expression{
											ast.NewIntLiteral(1),
											ast.NewStringLiteral("two"),
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with multiple statements",
			src:  `func main() { var x: int; var y: string; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "x", Type: types.Int},
								&ast.VariableDeclaration{Name: "y", Type: types.String},
							},
						},
					},
				},
			},
		},
		{
			name: "program with two empty functions",
			src:  `func main() {} func helper() {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
					{
						Name: "helper",
						Args: []ast.Arg{},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with return without value",
			src:  `func main() { return; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ReturnStatement{Value: nil},
							},
						},
					},
				},
			},
		},
		{
			name: "function with return with integer value",
			src:  `func main() { return 42; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ReturnStatement{Value: ast.NewIntLiteral(42)},
							},
						},
					},
				},
			},
		},
		{
			name: "function with return with string value",
			src:  `func main() { return "hello"; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ReturnStatement{
									Value: ast.NewStringLiteral("hello"),
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with return with function call",
			src:  `func main() { return foo(); }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ReturnStatement{
									Value: &ast.FunctionCall{
										FunctionName: "foo",
										Args:         []ast.Expression{},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with multiple return statements",
			src:  `func main() { return 1; return "two"; return; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ReturnStatement{
									Value: ast.NewIntLiteral(1),
								},
								&ast.ReturnStatement{
									Value: ast.NewStringLiteral("two"),
								},
								&ast.ReturnStatement{
									Value: nil,
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with mixed statements including return",
			src:  `func main() { var x: int; return x; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{
									Name: "x",
									Type: types.Int,
								},
								&ast.ReturnStatement{
									Value: &ast.VariableReference{
										Name: "x",
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with break statement",
			src:  `func main() { break; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.BreakStatement{},
							},
						},
					},
				},
			},
		},
		{
			name: "function with multiple statements including break",
			src:  `func main() { var x: int; break; return x; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{
									Name: "x",
									Type: types.Int,
								},
								&ast.BreakStatement{},
								&ast.ReturnStatement{
									Value: &ast.VariableReference{
										Name: "x",
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with continue statement",
			src:  `func main() { continue; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ContinueStatement{},
							},
						},
					},
				},
			},
		},
		{
			name: "function with multiple statements including continue",
			src:  `func main() { var x: int; continue; return x; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{
									Name: "x",
									Type: types.Int,
								},
								&ast.ContinueStatement{},
								&ast.ReturnStatement{
									Value: &ast.VariableReference{
										Name: "x",
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "struct declaration with two fields",
			src:  `struct Foo { x: int; y: string; }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name: "Foo",
						Fields: []ast.StructField{
							{Name: "x", Type: types.Int},
							{Name: "y", Type: types.String},
						},
					},
				},
			},
		},
		{
			name: "empty struct",
			src:  `struct Empty { }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name:   "Empty",
						Fields: []ast.StructField{},
					},
				},
			},
		},
		{
			name: "struct with single field",
			src:  `struct Point { x: int; }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name: "Point",
						Fields: []ast.StructField{
							{Name: "x", Type: types.Int},
						},
					},
				},
			},
		},
		{
			name: "multiple structs",
			src:  `struct Point { x: int; y: int; } struct Person { name: string; age: int; }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name: "Point",
						Fields: []ast.StructField{
							{Name: "x", Type: types.Int},
							{Name: "y", Type: types.Int},
						},
					},
					{
						Name: "Person",
						Fields: []ast.StructField{
							{Name: "name", Type: types.String},
							{Name: "age", Type: types.Int},
						},
					},
				},
			},
		},
		{
			name: "struct and function together",
			src:  `struct Point { x: int; y: int; } func main() { var p: Point; }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name: "Point",
						Fields: []ast.StructField{
							{Name: "x", Type: types.Int},
							{Name: "y", Type: types.Int},
						},
					},
				},
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "p", Type: types.NewBaseType("Point")},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}
			if !compareASTIgnoreLocation(prog, tc.expected) {
				t.Errorf("ParseProgram() got = %#v, want %#v", prog, tc.expected)
			}
		})
	}
}

func TestParseProgram_PointerTypes(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "function with pointer parameter",
			src:  `func test(ptr: *int) {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "test",
						Args: []ast.Arg{
							{Name: "ptr", Type: types.NewPointerType(types.Int)},
						},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with pointer return type",
			src:  `func getPtr(): *string {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:       "getPtr",
						Args:       []ast.Arg{},
						Body:       ast.Block{Statements: []ast.Statement{}},
						ReturnType: types.NewPointerType(types.String),
					},
				},
			},
		},
		{
			name: "function with pointer to pointer parameter",
			src:  `func test(ptr: **int) {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "test",
						Args: []ast.Arg{
							{Name: "ptr", Type: types.NewPointerType(types.NewPointerType(types.Int))},
						},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with multiple pointer parameters",
			src:  `func test(x: *int, y: *string, z: **bool) {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "test",
						Args: []ast.Arg{
							{Name: "x", Type: types.NewPointerType(types.Int)},
							{Name: "y", Type: types.NewPointerType(types.String)},
							{Name: "z", Type: types.NewPointerType(types.NewPointerType(types.Bool))},
						},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "variable declaration with pointer type",
			src:  `func main() { var ptr: *int; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "ptr", Type: types.NewPointerType(types.Int)},
							},
						},
					},
				},
			},
		},
		{
			name: "variable declaration with pointer to pointer type",
			src:  `func main() { var ptr: **string; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "ptr", Type: types.NewPointerType(types.NewPointerType(types.String))},
							},
						},
					},
				},
			},
		},
		{
			name: "extern function with pointer parameter",
			src:  `extern func malloc(size: *int): *int;`,
			expected: &ast.Program{
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "malloc",
						Args: []ast.Arg{
							{Name: "size", Type: types.NewPointerType(types.Int)},
						},
						ReturnType: types.NewPointerType(types.Int),
					},
				},
			},
		},
		{
			name: "struct with pointer field",
			src:  `struct Node { data: int; next: *Node; }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name: "Node",
						Fields: []ast.StructField{
							{Name: "data", Type: types.Int},
							{Name: "next", Type: types.NewPointerType(types.NewBaseType("Node"))},
						},
					},
				},
			},
		},
		{
			name: "struct with multiple pointer fields",
			src:  `struct Complex { value: *int; name: *string; parent: **Complex; }`,
			expected: &ast.Program{
				StructDeclarations: []ast.StructDeclaration{
					{
						Name: "Complex",
						Fields: []ast.StructField{
							{Name: "value", Type: types.NewPointerType(types.Int)},
							{Name: "name", Type: types.NewPointerType(types.String)},
							{Name: "parent", Type: types.NewPointerType(types.NewPointerType(types.NewBaseType("Complex")))},
						},
					},
				},
			},
		},
		{
			name: "mixed pointer types and regular types",
			src:  `func process(x: int, ptr: *int, y: string, ptrptr: **bool): *string {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "process",
						Args: []ast.Arg{
							{Name: "x", Type: types.Int},
							{Name: "ptr", Type: types.NewPointerType(types.Int)},
							{Name: "y", Type: types.String},
							{Name: "ptrptr", Type: types.NewPointerType(types.NewPointerType(types.Bool))},
						},
						Body:       ast.Block{Statements: []ast.Statement{}},
						ReturnType: types.NewPointerType(types.String),
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}
			if !compareASTIgnoreLocation(prog, tc.expected) {
				t.Errorf("ParseProgram() got = %#v, want %#v", prog, tc.expected)
			}
		})
	}
}

func TestParseProgram_Error(t *testing.T) {
	testCases := []struct {
		name          string
		src           string
		expectedError string
	}{
		{
			name:          "missing func keyword",
			src:           `main() {}`,
			expectedError: "1:1: expected 'struct', 'func' or 'extern'",
		},
		{
			name:          "missing function name",
			src:           `func() {}`,
			expectedError: "1:5: expected function name",
		},
		{
			name:          "missing opening parenthesis",
			src:           `func main) {}`,
			expectedError: "1:10: expected '('",
		},
		{
			name:          "missing closing parenthesis in function call",
			src:           `func main() { foo(1 }`,
			expectedError: "1:21: expected ',' or ')'",
		},
		{
			name:          "missing closing brace",
			src:           `func main() {`,
			expectedError: "unexpected EOF",
		},
		{
			name:          "incomplete var declaration",
			src:           `func main() { var x; }`,
			expectedError: "1:20: expected ':' after variable name",
		},
		{
			name:          "missing semicolon",
			src:           `func main() { var x: int }`,
			expectedError: "1:26: expected ';' after statement",
		},
		{
			name:          "missing colon in var declaration",
			src:           `func main() { var x int; }`,
			expectedError: "1:21: expected ':' after variable name",
		},
		{
			name:          "missing struct name",
			src:           `struct { x: int; }`,
			expectedError: "1:8: expected struct name",
		},
		{
			name:          "missing opening brace in struct",
			src:           `struct Foo x: int; }`,
			expectedError: "1:12: expected '{' after struct name",
		},
		{
			name:          "missing colon in struct field",
			src:           `struct Foo { x int; }`,
			expectedError: "1:16: expected ':' after field name",
		},
		{
			name:          "missing semicolon in struct field",
			src:           `struct Foo { x: int }`,
			expectedError: "1:21: expected ';' after field declaration",
		},
		{
			name:          "missing closing brace in struct",
			src:           `struct Foo { x: int;`,
			expectedError: "expected field name, got <EOF>",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Fatalf("ParseProgram() expected error, but got nil")
			}
			if !strings.Contains(err.Error(), tc.expectedError) {
				t.Errorf("ParseProgram() error = %q, want error containing %q", err.Error(), tc.expectedError)
			}
		})
	}
}

func TestParseExpression_FunctionCall(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name: "function call with no arguments",
			src:  `func main() { foo(); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args:         []ast.Expression{},
			},
		},
		{
			name: "function call with single integer argument",
			src:  `func main() { foo(42); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args: []ast.Expression{
					ast.NewIntLiteral(42),
				},
			},
		},
		{
			name: "function call with single string argument",
			src:  `func main() { foo("hello"); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args: []ast.Expression{
					ast.NewStringLiteral("hello"),
				},
			},
		},
		{
			name: "function call with multiple arguments",
			src:  `func main() { foo(1, "two", 3); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args: []ast.Expression{
					ast.NewIntLiteral(1),
					ast.NewStringLiteral("two"),
					ast.NewIntLiteral(3),
				},
			},
		},
		{
			name: "nested function calls",
			src:  `func main() { foo(bar()); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args: []ast.Expression{
					&ast.FunctionCall{
						FunctionName: "bar",
						Args:         []ast.Expression{},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}

			// Extract the expression from the program structure
			if len(prog.Functions) != 1 {
				t.Fatalf("Expected 1 function, got %d", len(prog.Functions))
			}
			if len(prog.Functions[0].Body.Statements) != 1 {
				t.Fatalf("Expected 1 statement, got %d", len(prog.Functions[0].Body.Statements))
			}
			stmt := prog.Functions[0].Body.Statements[0]
			exprStmt, ok := stmt.(*ast.ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !compareASTIgnoreLocation(exprStmt.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, tc.expected)
			}
		})
	}
}

func TestParseExpression_IntegerLiteral(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name:     "zero",
			src:      `func main() { 0; }`,
			expected: ast.NewIntLiteral(0),
		},
		{
			name:     "positive integer",
			src:      `func main() { 42; }`,
			expected: ast.NewIntLiteral(42),
		},
		{
			name:     "large integer",
			src:      `func main() { 999999; }`,
			expected: ast.NewIntLiteral(999999),
		},
		{
			name:     "single digit",
			src:      `func main() { 7; }`,
			expected: ast.NewIntLiteral(7),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}

			// Extract the expression from the program structure
			if len(prog.Functions) != 1 {
				t.Fatalf("Expected 1 function, got %d", len(prog.Functions))
			}
			if len(prog.Functions[0].Body.Statements) != 1 {
				t.Fatalf("Expected 1 statement, got %d", len(prog.Functions[0].Body.Statements))
			}
			stmt := prog.Functions[0].Body.Statements[0]
			exprStmt, ok := stmt.(*ast.ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !compareASTIgnoreLocation(exprStmt.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, tc.expected)
			}
		})
	}
}

func TestParseExpression_StringLiteral(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name:     "empty string",
			src:      `func main() { ""; }`,
			expected: ast.NewStringLiteral(""),
		},
		{
			name:     "simple string",
			src:      `func main() { "hello"; }`,
			expected: ast.NewStringLiteral("hello"),
		},
		{
			name:     "string with spaces",
			src:      `func main() { "hello world"; }`,
			expected: ast.NewStringLiteral("hello world"),
		},
		{
			name:     "string with numbers",
			src:      `func main() { "abc123"; }`,
			expected: ast.NewStringLiteral("abc123"),
		},
		{
			name:     "string with special characters",
			src:      `func main() { "hello, world!"; }`,
			expected: ast.NewStringLiteral("hello, world!"),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}

			// Extract the expression from the program structure
			if len(prog.Functions) != 1 {
				t.Fatalf("Expected 1 function, got %d", len(prog.Functions))
			}
			if len(prog.Functions[0].Body.Statements) != 1 {
				t.Fatalf("Expected 1 statement, got %d", len(prog.Functions[0].Body.Statements))
			}
			stmt := prog.Functions[0].Body.Statements[0]
			exprStmt, ok := stmt.(*ast.ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !compareASTIgnoreLocation(exprStmt.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, tc.expected)
			}
		})
	}
}

func TestParseExpression_Error(t *testing.T) {
	testCases := []struct {
		name          string
		src           string
		expectedError string
	}{
		{
			name:          "function call with malformed argument",
			src:           `func main() { foo(123abc); }`,
			expectedError: "expected ',' or ')'",
		},
		{
			name:          "function call missing closing parenthesis",
			src:           `func main() { foo(1; }`,
			expectedError: "expected ',' or ')'",
		},
		{
			name:          "function call with invalid comma placement",
			src:           `func main() { foo(,1); }`,
			expectedError: "unknown expression",
		},
		{
			name:          "function call missing opening parenthesis",
			src:           `func main() { foo 1); }`,
			expectedError: "expected ';' after statement",
		},
		{
			name:          "empty expression",
			src:           `func main() { ; }`,
			expectedError: "unknown expression",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Fatalf("ParseProgram() expected error, but got nil")
			}
			if !strings.Contains(err.Error(), tc.expectedError) {
				t.Errorf("ParseProgram() error = %q, want error containing %q", err.Error(), tc.expectedError)
			}
		})
	}
}

func TestParseExpression_Assignment(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name: "assignment with integer literal",
			src:  `func main() { x = 42; }`,
			expected: &ast.Assignment{
				VariableName: "x",
				Value:        ast.NewIntLiteral(42),
			},
		},
		{
			name: "assignment with string literal",
			src:  `func main() { name = "hello"; }`,
			expected: &ast.Assignment{
				VariableName: "name",
				Value:        ast.NewStringLiteral("hello"),
			},
		},
		{
			name: "assignment with function call",
			src:  `func main() { result = foo(); }`,
			expected: &ast.Assignment{
				VariableName: "result",
				Value: &ast.FunctionCall{
					FunctionName: "foo",
					Args:         []ast.Expression{},
				},
			},
		},
		{
			name: "assignment with function call with args",
			src:  `func main() { result = add(1, 2); }`,
			expected: &ast.Assignment{
				VariableName: "result",
				Value: &ast.FunctionCall{
					FunctionName: "add",
					Args: []ast.Expression{
						ast.NewIntLiteral(1),
						ast.NewIntLiteral(2),
					},
				},
			},
		},
		{
			name: "assignment with zero",
			src:  `func main() { counter = 0; }`,
			expected: &ast.Assignment{
				VariableName: "counter",
				Value:        ast.NewIntLiteral(0),
			},
		},
		{
			name: "assignment with empty string",
			src:  `func main() { text = ""; }`,
			expected: &ast.Assignment{
				VariableName: "text",
				Value:        ast.NewStringLiteral(""),
			},
		},
		{
			name: "chained assignment",
			src:  `func main() { x = y = 1; }`,
			expected: &ast.Assignment{
				VariableName: "x",
				Value: &ast.Assignment{
					VariableName: "y",
					Value:        ast.NewIntLiteral(1),
				},
			},
		},
		{
			name: "assignment with variable",
			src:  `func main() { x = y; }`,
			expected: &ast.Assignment{
				VariableName: "x",
				Value: &ast.VariableReference{
					Name: "y",
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}

			// Extract the expression from the program structure
			if len(prog.Functions) != 1 {
				t.Fatalf("Expected 1 function, got %d", len(prog.Functions))
			}
			if len(prog.Functions[0].Body.Statements) != 1 {
				t.Fatalf("Expected 1 statement, got %d", len(prog.Functions[0].Body.Statements))
			}
			stmt := prog.Functions[0].Body.Statements[0]
			exprStmt, ok := stmt.(*ast.ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !compareASTIgnoreLocation(exprStmt.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, tc.expected)
			}
		})
	}
}

func TestParseExpression_Assignment_Error(t *testing.T) {
	testCases := []struct {
		name          string
		src           string
		expectedError string
	}{
		{
			name:          "assignment missing equals sign",
			src:           `func main() { x 42; }`,
			expectedError: "expected ';' after statement",
		},
		{
			name:          "assignment missing value",
			src:           `func main() { x = ; }`,
			expectedError: "unknown expression",
		},
		{
			name:          "assignment with invalid variable name",
			src:           `func main() { 123 = 42; }`,
			expectedError: "expected ';' after statement",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Fatalf("ParseProgram() expected error, but got nil")
			}
			if !strings.Contains(err.Error(), tc.expectedError) {
				t.Errorf("ParseProgram() error = %q, want error containing %q", err.Error(), tc.expectedError)
			}
		})
	}
}

func TestParseExpression_AddressOf_Error(t *testing.T) {
	testCases := []struct {
		name          string
		src           string
		expectedError string
	}{
		{
			name:          "address-of with function call",
			src:           `func main() { &foo(); }`,
			expectedError: "expected ';' after statement",
		},
		{
			name:          "address-of with literal",
			src:           `func main() { &42; }`,
			expectedError: "expected variable name",
		},
		{
			name:          "address-of with string literal",
			src:           `func main() { &"hello"; }`,
			expectedError: "expected variable name",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Fatalf("ParseProgram() expected error, but got nil")
			}
			if !strings.Contains(err.Error(), tc.expectedError) {
				t.Errorf("ParseProgram() error = %q, want error containing %q", err.Error(), tc.expectedError)
			}
		})
	}
}

func TestParseExpression_BinaryOperation(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name: "simple addition",
			src:  "1 + 2",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(1),
				Operator: "+",
				Right:    ast.NewIntLiteral(2),
			},
		},
		{
			name: "addition with variables",
			src:  "x + y",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "x"},
				Operator: "+",
				Right:    &ast.VariableReference{Name: "y"},
			},
		},
		{
			name: "addition with mixed types",
			src:  "5 + x",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(5),
				Operator: "+",
				Right:    &ast.VariableReference{Name: "x"},
			},
		},
		{
			name: "simple subtraction",
			src:  "10 - 3",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(10),
				Operator: "-",
				Right:    ast.NewIntLiteral(3),
			},
		},
		{
			name: "subtraction with variables",
			src:  "x - y",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "x"},
				Operator: "-",
				Right:    &ast.VariableReference{Name: "y"},
			},
		},
		{
			name: "subtraction with mixed types",
			src:  "15 - z",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(15),
				Operator: "-",
				Right:    &ast.VariableReference{Name: "z"},
			},
		},
		{
			name: "variable subtraction from literal",
			src:  "a - 5",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "a"},
				Operator: "-",
				Right:    ast.NewIntLiteral(5),
			},
		},
		{
			name: "simple multiplication",
			src:  "3 * 4",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(3),
				Operator: "*",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "multiplication with variables",
			src:  "x * y",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "x"},
				Operator: "*",
				Right:    &ast.VariableReference{Name: "y"},
			},
		},
		{
			name: "multiplication with mixed types",
			src:  "7 * z",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(7),
				Operator: "*",
				Right:    &ast.VariableReference{Name: "z"},
			},
		},
		{
			name: "variable multiplication with literal",
			src:  "b * 8",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "b"},
				Operator: "*",
				Right:    ast.NewIntLiteral(8),
			},
		},
		{
			name: "mixed addition and multiplication (correct precedence)",
			src:  "2 + 3 * 4",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(2),
				Operator: "+",
				Right: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(3),
					Operator: "*",
					Right:    ast.NewIntLiteral(4),
				},
			},
		},
		{
			name: "mixed multiplication and addition (correct precedence)",
			src:  "2 * 3 + 4",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "*",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "+",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "multiple multiplications (left associative)",
			src:  "2 * 3 * 4",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "*",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "*",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "complex precedence test",
			src:  "1 + 2 * 3 + 4",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(1),
					Operator: "+",
					Right: &ast.BinaryOperation{
						Left:     ast.NewIntLiteral(2),
						Operator: "*",
						Right:    ast.NewIntLiteral(3),
					},
				},
				Operator: "+",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "subtraction and multiplication precedence",
			src:  "10 - 2 * 3",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(10),
				Operator: "-",
				Right: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "*",
					Right:    ast.NewIntLiteral(3),
				},
			},
		},
		{
			name: "simple division",
			src:  "12 / 3",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(12),
				Operator: "/",
				Right:    ast.NewIntLiteral(3),
			},
		},
		{
			name: "division with variables",
			src:  "x / y",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "x"},
				Operator: "/",
				Right:    &ast.VariableReference{Name: "y"},
			},
		},
		{
			name: "division with mixed types",
			src:  "20 / z",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(20),
				Operator: "/",
				Right:    &ast.VariableReference{Name: "z"},
			},
		},
		{
			name: "variable division with literal",
			src:  "w / 4",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "w"},
				Operator: "/",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "mixed addition and division (correct precedence)",
			src:  "2 + 8 / 4",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(2),
				Operator: "+",
				Right: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(8),
					Operator: "/",
					Right:    ast.NewIntLiteral(4),
				},
			},
		},
		{
			name: "mixed division and addition (correct precedence)",
			src:  "12 / 3 + 4",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(12),
					Operator: "/",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "+",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "multiplication and division (same precedence, left associative)",
			src:  "8 * 2 / 4",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(8),
					Operator: "*",
					Right:    ast.NewIntLiteral(2),
				},
				Operator: "/",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "division and multiplication (same precedence, left associative)",
			src:  "12 / 3 * 2",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(12),
					Operator: "/",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "*",
				Right:    ast.NewIntLiteral(2),
			},
		},
		{
			name: "multiple divisions (left associative)",
			src:  "24 / 4 / 2",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(24),
					Operator: "/",
					Right:    ast.NewIntLiteral(4),
				},
				Operator: "/",
				Right:    ast.NewIntLiteral(2),
			},
		},
		{
			name: "complex expression with division",
			src:  "1 + 12 / 3 - 2",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(1),
					Operator: "+",
					Right: &ast.BinaryOperation{
						Left:     ast.NewIntLiteral(12),
						Operator: "/",
						Right:    ast.NewIntLiteral(3),
					},
				},
				Operator: "-",
				Right:    ast.NewIntLiteral(2),
			},
		},
		{
			name: "simple parentheses",
			src:  "(2 + 3)",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(2),
				Operator: "+",
				Right:    ast.NewIntLiteral(3),
			},
		},
		{
			name: "parentheses changing precedence",
			src:  "(2 + 3) * 4",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "+",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "*",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "parentheses with division",
			src:  "12 / (2 + 1)",
			expected: &ast.BinaryOperation{
				Left:     ast.NewIntLiteral(12),
				Operator: "/",
				Right: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "+",
					Right:    ast.NewIntLiteral(1),
				},
			},
		},
		{
			name: "nested parentheses",
			src:  "((2 + 3) * 4)",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "+",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "*",
				Right:    ast.NewIntLiteral(4),
			},
		},
		{
			name: "complex parentheses expression",
			src:  "(2 + 3) * (4 - 1)",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(2),
					Operator: "+",
					Right:    ast.NewIntLiteral(3),
				},
				Operator: "*",
				Right: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(4),
					Operator: "-",
					Right:    ast.NewIntLiteral(1),
				},
			},
		},
		{
			name: "parentheses with variables",
			src:  "(x + y) * z",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "x"},
					Operator: "+",
					Right:    &ast.VariableReference{Name: "y"},
				},
				Operator: "*",
				Right:    &ast.VariableReference{Name: "z"},
			},
		},
		{
			name: "multiple operations with parentheses",
			src:  "1 + (2 * 3) + (8 / 4)",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(1),
					Operator: "+",
					Right: &ast.BinaryOperation{
						Left:     ast.NewIntLiteral(2),
						Operator: "*",
						Right:    ast.NewIntLiteral(3),
					},
				},
				Operator: "+",
				Right: &ast.BinaryOperation{
					Left:     ast.NewIntLiteral(8),
					Operator: "/",
					Right:    ast.NewIntLiteral(4),
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			result, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if !compareASTIgnoreLocation(result, tc.expected) {
				t.Errorf("expected %+v, got %+v", tc.expected, result)
			}
		})
	}
}

func TestParseExpression_BooleanOperators(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		// Comparison operators
		{
			name: "equality comparison",
			src:  "x == 42",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "x"},
				Operator: "==",
				Right:    ast.NewIntLiteral(42),
			},
		},
		{
			name: "inequality comparison",
			src:  "y != 0",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "y"},
				Operator: "!=",
				Right:    ast.NewIntLiteral(0),
			},
		},
		{
			name: "less than comparison",
			src:  "a < b",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "a"},
				Operator: "<",
				Right:    &ast.VariableReference{Name: "b"},
			},
		},
		{
			name: "greater than comparison",
			src:  "x > 10",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "x"},
				Operator: ">",
				Right:    ast.NewIntLiteral(10),
			},
		},
		{
			name: "less than or equal comparison",
			src:  "score <= max",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "score"},
				Operator: "<=",
				Right:    &ast.VariableReference{Name: "max"},
			},
		},
		{
			name: "greater than or equal comparison",
			src:  "age >= 18",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "age"},
				Operator: ">=",
				Right:    ast.NewIntLiteral(18),
			},
		},
		// Logical operators
		{
			name: "logical AND",
			src:  "x > 0 && y > 0",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "x"},
					Operator: ">",
					Right:    ast.NewIntLiteral(0),
				},
				Operator: "&&",
				Right: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "y"},
					Operator: ">",
					Right:    ast.NewIntLiteral(0),
				},
			},
		},
		{
			name: "logical OR",
			src:  "x == 0 || y == 0",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "x"},
					Operator: "==",
					Right:    ast.NewIntLiteral(0),
				},
				Operator: "||",
				Right: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "y"},
					Operator: "==",
					Right:    ast.NewIntLiteral(0),
				},
			},
		},
		// Unary negation operator
		{
			name: "logical NOT",
			src:  "!found",
			expected: &ast.UnaryOperation{
				Operator: "!",
				Operand:  &ast.VariableReference{Name: "found"},
			},
		},
		{
			name: "logical NOT with parentheses",
			src:  "!(x > 0)",
			expected: &ast.UnaryOperation{
				Operator: "!",
				Operand: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "x"},
					Operator: ">",
					Right:    ast.NewIntLiteral(0),
				},
			},
		},
		{
			name: "address-of operator",
			src:  "&x",
			expected: &ast.UnaryOperation{
				Operator: "&",
				Operand:  &ast.VariableReference{Name: "x"},
			},
		},
		// Complex expressions with mixed precedence
		{
			name: "comparison with arithmetic (correct precedence)",
			src:  "x + 1 == y * 2",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "x"},
					Operator: "+",
					Right:    ast.NewIntLiteral(1),
				},
				Operator: "==",
				Right: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "y"},
					Operator: "*",
					Right:    ast.NewIntLiteral(2),
				},
			},
		},
		{
			name: "logical AND with higher precedence than OR",
			src:  "a || b && c",
			expected: &ast.BinaryOperation{
				Left:     &ast.VariableReference{Name: "a"},
				Operator: "||",
				Right: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "b"},
					Operator: "&&",
					Right:    &ast.VariableReference{Name: "c"},
				},
			},
		},
		{
			name: "negation with comparison",
			src:  "!x == 0",
			expected: &ast.BinaryOperation{
				Left: &ast.UnaryOperation{
					Operator: "!",
					Operand:  &ast.VariableReference{Name: "x"},
				},
				Operator: "==",
				Right:    ast.NewIntLiteral(0),
			},
		},
		{
			name: "complex boolean expression",
			src:  "x > 0 && y < 10 || z == 42",
			expected: &ast.BinaryOperation{
				Left: &ast.BinaryOperation{
					Left: &ast.BinaryOperation{
						Left:     &ast.VariableReference{Name: "x"},
						Operator: ">",
						Right:    ast.NewIntLiteral(0),
					},
					Operator: "&&",
					Right: &ast.BinaryOperation{
						Left:     &ast.VariableReference{Name: "y"},
						Operator: "<",
						Right:    ast.NewIntLiteral(10),
					},
				},
				Operator: "||",
				Right: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "z"},
					Operator: "==",
					Right:    ast.NewIntLiteral(42),
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			result, err := parser.parseExpression()
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if !compareASTIgnoreLocation(result, tc.expected) {
				t.Errorf("expected %+v, got %+v", tc.expected, result)
			}
		})
	}
}

func TestParseStatement_IfStatement(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "simple if statement without else",
			src:  `func main() { if x == 5 { return; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: "==",
										Right:    ast.NewIntLiteral(5),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{Value: nil},
										},
									},
									ElseBlock: nil,
								},
							},
						},
					},
				},
			},
		},
		{
			name: "if statement with else",
			src:  `func main() { if x > 0 { y = 1; } else { y = 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													VariableName: "y",
													Value:        ast.NewIntLiteral(1),
												},
											},
										},
									},
									ElseBlock: &ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													VariableName: "y",
													Value:        ast.NewIntLiteral(0),
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "if statement with complex boolean condition",
			src:  `func main() { if (x > 0) && (y < 10) { return x; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left: &ast.BinaryOperation{
											Left:     &ast.VariableReference{Name: "x"},
											Operator: ">",
											Right:    ast.NewIntLiteral(0),
										},
										Operator: "&&",
										Right: &ast.BinaryOperation{
											Left:     &ast.VariableReference{Name: "y"},
											Operator: "<",
											Right:    ast.NewIntLiteral(10),
										},
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{
												Value: &ast.VariableReference{Name: "x"},
											},
										},
									},
									ElseBlock: nil,
								},
							},
						},
					},
				},
			},
		},
		{
			name: "if statement with unary not condition",
			src:  `func main() { if !flag { return; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.UnaryOperation{
										Operator: "!",
										Operand:  &ast.VariableReference{Name: "flag"},
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{Value: nil},
										},
									},
									ElseBlock: nil,
								},
							},
						},
					},
				},
			},
		},
		{
			name: "nested if statements",
			src:  `func main() { if x > 0 { if y > 0 { return 1; } } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "y"},
													Operator: ">",
													Right:    ast.NewIntLiteral(0),
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(1),
														},
													},
												},
												ElseBlock: nil,
											},
										},
									},
									ElseBlock: nil,
								},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			program, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("Error parsing program: %v", err)
			}
			if !compareASTIgnoreLocation(program, tc.expected) {
				t.Errorf("Expected %+v, got %+v", tc.expected, program)
			}
		})
	}
}

func TestParseStatement_IfStatement_Error(t *testing.T) {
	testCases := []struct {
		name string
		src  string
	}{
		{
			name: "if without condition",
			src:  `func main() { if { return; } }`,
		},
		{
			name: "if without opening brace",
			src:  `func main() { if x > 0  return; } }`,
		},
		{
			name: "if without closing brace",
			src:  `func main() { if x > 0 { return; }`,
		},
		{
			name: "else without opening brace",
			src:  `func main() { if x > 0 { return; } else  return; } }`,
		},
		{
			name: "else without closing brace",
			src:  `func main() { if x > 0 { return; } else { return; }`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Error("Expected error but got none")
			}
		})
	}
}

func TestParseStatement_ElseIfStatement(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "simple else if",
			src:  `func main() { if x == 1 { return 1; } else if x == 2 { return 2; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: "==",
										Right:    ast.NewIntLiteral(1),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{
												Value: ast.NewIntLiteral(1),
											},
										},
									},
									ElseBlock: &ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "x"},
													Operator: "==",
													Right:    ast.NewIntLiteral(2),
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(2),
														},
													},
												},
												ElseBlock: nil,
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "else if with final else",
			src:  `func main() { if x == 1 { return 1; } else if x == 2 { return 2; } else { return 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: "==",
										Right:    ast.NewIntLiteral(1),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{
												Value: ast.NewIntLiteral(1),
											},
										},
									},
									ElseBlock: &ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "x"},
													Operator: "==",
													Right:    ast.NewIntLiteral(2),
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(2),
														},
													},
												},
												ElseBlock: &ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(0),
														},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "multiple else if chain",
			src:  `func main() { if x == 1 { return 1; } else if x == 2 { return 2; } else if x == 3 { return 3; } else { return 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: "==",
										Right:    ast.NewIntLiteral(1),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{
												Value: ast.NewIntLiteral(1),
											},
										},
									},
									ElseBlock: &ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "x"},
													Operator: "==",
													Right:    ast.NewIntLiteral(2),
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(2),
														},
													},
												},
												ElseBlock: &ast.Block{
													Statements: []ast.Statement{
														&ast.IfStatement{
															Condition: &ast.BinaryOperation{
																Left:     &ast.VariableReference{Name: "x"},
																Operator: "==",
																Right:    ast.NewIntLiteral(3),
															},
															ThenBlock: ast.Block{
																Statements: []ast.Statement{
																	&ast.ReturnStatement{
																		Value: ast.NewIntLiteral(3),
																	},
																},
															},
															ElseBlock: &ast.Block{
																Statements: []ast.Statement{
																	&ast.ReturnStatement{
																		Value: ast.NewIntLiteral(0),
																	},
																},
															},
														},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "else if with complex condition",
			src:  `func main() { if x > 10 { return 1; } else if (x > 5) && (x <= 10) { return 2; } else { return 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.IfStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(10),
									},
									ThenBlock: ast.Block{
										Statements: []ast.Statement{
											&ast.ReturnStatement{
												Value: ast.NewIntLiteral(1),
											},
										},
									},
									ElseBlock: &ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left: &ast.BinaryOperation{
														Left:     &ast.VariableReference{Name: "x"},
														Operator: ">",
														Right:    ast.NewIntLiteral(5),
													},
													Operator: "&&",
													Right: &ast.BinaryOperation{
														Left:     &ast.VariableReference{Name: "x"},
														Operator: "<=",
														Right:    ast.NewIntLiteral(10),
													},
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(2),
														},
													},
												},
												ElseBlock: &ast.Block{
													Statements: []ast.Statement{
														&ast.ReturnStatement{
															Value: ast.NewIntLiteral(0),
														},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			program, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("Error parsing program: %v", err)
			}
			if !compareASTIgnoreLocation(program, tc.expected) {
				t.Errorf("Expected %+v, got %+v", tc.expected, program)
			}
		})
	}
}

func TestParseStatement_WhileStatement(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "simple while loop",
			src:  `func main() { while x > 0 { x = x - 1; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													VariableName: "x",
													Value: &ast.BinaryOperation{
														Left:     &ast.VariableReference{Name: "x"},
														Operator: "-",
														Right:    ast.NewIntLiteral(1),
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "while loop with complex condition",
			src:  `func main() { while (x > 0) && (y < 10) { printf("loop\n"); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left: &ast.BinaryOperation{
											Left:     &ast.VariableReference{Name: "x"},
											Operator: ">",
											Right:    ast.NewIntLiteral(0),
										},
										Operator: "&&",
										Right: &ast.BinaryOperation{
											Left:     &ast.VariableReference{Name: "y"},
											Operator: "<",
											Right:    ast.NewIntLiteral(10),
										},
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.FunctionCall{
													FunctionName: "printf",
													Args: []ast.Expression{
														ast.NewStringLiteral("loop\n"),
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "while loop with unary not condition",
			src:  `func main() { while !done { done = check(); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.UnaryOperation{
										Operator: "!",
										Operand:  &ast.VariableReference{Name: "done"},
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													VariableName: "done",
													Value: &ast.FunctionCall{
														FunctionName: "check",
														Args:         []ast.Expression{},
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "nested while loops",
			src:  `func main() { while x > 0 { while y > 0 { y = y - 1; } x = x - 1; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.WhileStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "y"},
													Operator: ">",
													Right:    ast.NewIntLiteral(0),
												},
												Body: ast.Block{
													Statements: []ast.Statement{
														&ast.ExpressionStatement{
															Expression: &ast.Assignment{
																VariableName: "y",
																Value: &ast.BinaryOperation{
																	Left:     &ast.VariableReference{Name: "y"},
																	Operator: "-",
																	Right:    ast.NewIntLiteral(1),
																},
															},
														},
													},
												},
											},
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													VariableName: "x",
													Value: &ast.BinaryOperation{
														Left:     &ast.VariableReference{Name: "x"},
														Operator: "-",
														Right:    ast.NewIntLiteral(1),
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "while loop with multiple statements in body",
			src:  `func main() { while i < 10 { printf("i = %d\n", i); i = i + 1; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "i"},
										Operator: "<",
										Right:    ast.NewIntLiteral(10),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.FunctionCall{
													FunctionName: "printf",
													Args: []ast.Expression{
														ast.NewStringLiteral("i = %d\n"),
														&ast.VariableReference{Name: "i"},
													},
												},
											},
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													VariableName: "i",
													Value: &ast.BinaryOperation{
														Left:     &ast.VariableReference{Name: "i"},
														Operator: "+",
														Right:    ast.NewIntLiteral(1),
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			program, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("Error parsing program: %v", err)
			}
			if !compareASTIgnoreLocation(program, tc.expected) {
				t.Errorf("Expected %+v, got %+v", tc.expected, program)
			}
		})
	}
}

func TestParseStatement_WhileStatement_Error(t *testing.T) {
	testCases := []struct {
		name string
		src  string
	}{
		{
			name: "while without condition",
			src:  `func main() { while { printf("loop\n"); } }`,
		},
		{
			name: "while without opening brace",
			src:  `func main() { while x > 0  printf("loop\n"); } }`,
		},
		{
			name: "while without closing brace",
			src:  `func main() { while x > 0 { printf("loop\n"); }`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Error("Expected error but got none")
			}
		})
	}
}

func TestParseStatement_BreakStatement(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "simple break statement",
			src:  `func main() { break; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.BreakStatement{},
							},
						},
					},
				},
			},
		},
		{
			name: "break statement in while loop",
			src:  `func main() { while x > 0 { break; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.BreakStatement{},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "break statement with other statements",
			src:  `func main() { while x > 0 { printf("before break\n"); break; printf("after break\n"); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.FunctionCall{
													FunctionName: "printf",
													Args: []ast.Expression{
														ast.NewStringLiteral("before break\n"),
													},
												},
											},
											&ast.BreakStatement{},
											&ast.ExpressionStatement{
												Expression: &ast.FunctionCall{
													FunctionName: "printf",
													Args: []ast.Expression{
														ast.NewStringLiteral("after break\n"),
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "multiple break statements",
			src:  `func main() { while x > 0 { if x == 5 { break; } break; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "x"},
													Operator: "==",
													Right:    ast.NewIntLiteral(5),
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.BreakStatement{},
													},
												},
												ElseBlock: nil,
											},
											&ast.BreakStatement{},
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			program, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("Error parsing program: %v", err)
			}
			if !compareASTIgnoreLocation(program, tc.expected) {
				t.Errorf("Expected %+v, got %+v", tc.expected, program)
			}
		})
	}
}

func TestParseStatement_ContinueStatement(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "simple continue statement",
			src:  `func main() { continue; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.ContinueStatement{},
							},
						},
					},
				},
			},
		},
		{
			name: "continue statement in while loop",
			src:  `func main() { while x > 0 { continue; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ContinueStatement{},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "continue statement with other statements",
			src:  `func main() { while x > 0 { printf("before continue\n"); continue; printf("after continue\n"); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.FunctionCall{
													FunctionName: "printf",
													Args: []ast.Expression{
														ast.NewStringLiteral("before continue\n"),
													},
												},
											},
											&ast.ContinueStatement{},
											&ast.ExpressionStatement{
												Expression: &ast.FunctionCall{
													FunctionName: "printf",
													Args: []ast.Expression{
														ast.NewStringLiteral("after continue\n"),
													},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "continue and break statements together",
			src:  `func main() { while x > 0 { if x == 5 { continue; } break; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{
							Statements: []ast.Statement{
								&ast.WhileStatement{
									Condition: &ast.BinaryOperation{
										Left:     &ast.VariableReference{Name: "x"},
										Operator: ">",
										Right:    ast.NewIntLiteral(0),
									},
									Body: ast.Block{
										Statements: []ast.Statement{
											&ast.IfStatement{
												Condition: &ast.BinaryOperation{
													Left:     &ast.VariableReference{Name: "x"},
													Operator: "==",
													Right:    ast.NewIntLiteral(5),
												},
												ThenBlock: ast.Block{
													Statements: []ast.Statement{
														&ast.ContinueStatement{},
													},
												},
												ElseBlock: nil,
											},
											&ast.BreakStatement{},
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src))
			parser := New(lex)
			program, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("Error parsing program: %v", err)
			}
			if !compareASTIgnoreLocation(program, tc.expected) {
				t.Errorf("Expected %+v, got %+v", tc.expected, program)
			}
		})
	}
}
