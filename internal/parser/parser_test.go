package parser

import (
	"reflect"
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
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
			src:  `extern func main() {}`,
			expected: &ast.Program{
				Loc: ast.Location{Line: 1, Col: 1},
				Functions: []ast.Function{
					{
						Loc:      ast.Location{Line: 1, Col: 1},
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body:     &ast.Block{Loc: ast.Location{Line: 1, Col: 14}, Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with var declaration",
			src:  `extern func main() { var x: int; }`,
			expected: &ast.Program{
				Loc: ast.Location{Line: 1, Col: 1},
				Functions: []ast.Function{
					{
						Loc:      ast.Location{Line: 1, Col: 1},
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Loc: ast.Location{Line: 1, Col: 15},
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Loc: ast.Location{Line: 1, Col: 15}, Name: "x", Type: ast.Int},
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
							{Name: "a", Type: ast.Int},
							{Name: "b", Type: ast.Int},
						},
						Body: &ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with expression statements",
			src:  `extern func main() { foo(1, "two"); }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { var x: int; var y: string; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "x", Type: ast.Int},
								&ast.VariableDeclaration{Name: "y", Type: ast.String},
							},
						},
					},
				},
			},
		},
		{
			name: "program with two empty functions",
			src:  `extern func main() {} func helper() {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body:     &ast.Block{Statements: []ast.Statement{}},
					},
					{
						Name: "helper",
						Args: []ast.Arg{},
						Body: &ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "function with return without value",
			src:  `extern func main() { return; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { return 42; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { return "hello"; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { return foo(); }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { return 1; return "two"; return; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { var x: int; return x; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{
									Name: "x",
									Type: ast.Int,
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
			src:  `extern func main() { break; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { var x: int; break; return x; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{
									Name: "x",
									Type: ast.Int,
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
			src:  `extern func main() { continue; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { var x: int; continue; return x; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{
									Name: "x",
									Type: ast.Int,
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
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
						Name: "Foo",
						Fields: []ast.StructField{
							{Name: "x", Type: ast.Int},
							{Name: "y", Type: ast.String},
						},
					},
				},
			},
		},
		{
			name: "empty struct",
			src:  `struct Empty { }`,
			expected: &ast.Program{
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
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
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
						Name: "Point",
						Fields: []ast.StructField{
							{Name: "x", Type: ast.Int},
						},
					},
				},
			},
		},
		{
			name: "multiple structs",
			src:  `struct Point { x: int; y: int; } struct Person { name: string; age: int; }`,
			expected: &ast.Program{
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
						Name: "Point",
						Fields: []ast.StructField{
							{Name: "x", Type: ast.Int},
							{Name: "y", Type: ast.Int},
						},
					},
					&ast.StructDeclaration{
						Name: "Person",
						Fields: []ast.StructField{
							{Name: "name", Type: ast.String},
							{Name: "age", Type: ast.Int},
						},
					},
				},
			},
		},
		{
			name: "struct and function together",
			src:  `struct Point { x: int; y: int; } extern func main() { var p: Point; }`,
			expected: &ast.Program{
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
						Name: "Point",
						Fields: []ast.StructField{
							{Name: "x", Type: ast.Int},
							{Name: "y", Type: ast.Int},
						},
					},
				},
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "p", Type: ast.NewBaseType("Point")},
							},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
							{Name: "ptr", Type: ast.NewPointerType(ast.Int)},
						},
						Body: &ast.Block{Statements: []ast.Statement{}},
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
						Body:       &ast.Block{Statements: []ast.Statement{}},
						ReturnType: ast.NewPointerType(ast.String),
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
							{Name: "ptr", Type: ast.NewPointerType(ast.NewPointerType(ast.Int))},
						},
						Body: &ast.Block{Statements: []ast.Statement{}},
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
							{Name: "x", Type: ast.NewPointerType(ast.Int)},
							{Name: "y", Type: ast.NewPointerType(ast.String)},
							{Name: "z", Type: ast.NewPointerType(ast.NewPointerType(ast.Bool))},
						},
						Body: &ast.Block{Statements: []ast.Statement{}},
					},
				},
			},
		},
		{
			name: "variable declaration with pointer type",
			src:  `extern func main() { var ptr: *int; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "ptr", Type: ast.NewPointerType(ast.Int)},
							},
						},
					},
				},
			},
		},
		{
			name: "variable declaration with pointer to pointer type",
			src:  `extern func main() { var ptr: **string; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
							Statements: []ast.Statement{
								&ast.VariableDeclaration{Name: "ptr", Type: ast.NewPointerType(ast.NewPointerType(ast.String))},
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
				Functions: []ast.Function{
					{
						Name: "malloc",
						Args: []ast.Arg{
							{Name: "size", Type: ast.NewPointerType(ast.Int)},
						},
						Body:       nil,
						ReturnType: ast.NewPointerType(ast.Int),
						External:   true,
					},
				},
			},
		},
		{
			name: "struct with pointer field",
			src:  `struct Node { data: int; next: *Node; }`,
			expected: &ast.Program{
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
						Name: "Node",
						Fields: []ast.StructField{
							{Name: "data", Type: ast.Int},
							{Name: "next", Type: ast.NewPointerType(ast.NewBaseType("Node"))},
						},
					},
				},
			},
		},
		{
			name: "struct with multiple pointer fields",
			src:  `struct Complex { value: *int; name: *string; parent: **Complex; }`,
			expected: &ast.Program{
				TypeDeclarations: []ast.TypeDeclaration{
					&ast.StructDeclaration{
						Name: "Complex",
						Fields: []ast.StructField{
							{Name: "value", Type: ast.NewPointerType(ast.Int)},
							{Name: "name", Type: ast.NewPointerType(ast.String)},
							{Name: "parent", Type: ast.NewPointerType(ast.NewPointerType(ast.NewBaseType("Complex")))},
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
							{Name: "x", Type: ast.Int},
							{Name: "ptr", Type: ast.NewPointerType(ast.Int)},
							{Name: "y", Type: ast.String},
							{Name: "ptrptr", Type: ast.NewPointerType(ast.NewPointerType(ast.Bool))},
						},
						Body:       &ast.Block{Statements: []ast.Statement{}},
						ReturnType: ast.NewPointerType(ast.String),
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:           `extern func main) {}`,
			expectedError: "1:17: expected '('",
		},
		{
			name:          "missing closing parenthesis in function call",
			src:           `extern func main() { foo(1 }`,
			expectedError: "1:28: expected ',' or ')'",
		},
		{
			name:          "missing closing brace",
			src:           `extern func main() {`,
			expectedError: "unexpected EOF",
		},
		{
			name:          "incomplete var declaration",
			src:           `extern func main() { var x; }`,
			expectedError: "1:27: expected ':' after variable name",
		},
		{
			name:          "missing semicolon",
			src:           `extern func main() { var x: int }`,
			expectedError: "1:33: expected ';' after statement",
		},
		{
			name:          "missing colon in var declaration",
			src:           `extern func main() { var x int; }`,
			expectedError: "1:28: expected ':' after variable name",
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { foo(); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args:         []ast.Expression{},
			},
		},
		{
			name: "function call with single integer argument",
			src:  `extern func main() { foo(42); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args: []ast.Expression{
					ast.NewIntLiteral(42),
				},
			},
		},
		{
			name: "function call with single string argument",
			src:  `extern func main() { foo("hello"); }`,
			expected: &ast.FunctionCall{
				FunctionName: "foo",
				Args: []ast.Expression{
					ast.NewStringLiteral("hello"),
				},
			},
		},
		{
			name: "function call with multiple arguments",
			src:  `extern func main() { foo(1, "two", 3); }`,
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
			src:  `extern func main() { foo(bar()); }`,
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:      `extern func main() { 0; }`,
			expected: ast.NewIntLiteral(0),
		},
		{
			name:     "positive integer",
			src:      `extern func main() { 42; }`,
			expected: ast.NewIntLiteral(42),
		},
		{
			name:     "large integer",
			src:      `extern func main() { 999999; }`,
			expected: ast.NewIntLiteral(999999),
		},
		{
			name:     "single digit",
			src:      `extern func main() { 7; }`,
			expected: ast.NewIntLiteral(7),
		},
		{
			name:     "hexadecimal lowercase",
			src:      `extern func main() { 0x42; }`,
			expected: ast.NewIntLiteral(0x42),
		},
		{
			name:     "hexadecimal uppercase",
			src:      `extern func main() { 0X42; }`,
			expected: ast.NewIntLiteral(0x42),
		},
		{
			name:     "hexadecimal with letters",
			src:      `extern func main() { 0xdeadl; }`,
			expected: ast.NewInt64Literal(0xdead),
		},
		{
			name:     "hexadecimal mixed case",
			src:      `extern func main() { 0XaBc; }`,
			expected: ast.NewIntLiteral(0xaBc),
		},
		{
			name:     "hexadecimal zero",
			src:      `extern func main() { 0x0; }`,
			expected: ast.NewIntLiteral(0x0),
		},
		{
			name:     "hexadecimal 64-bit",
			src:      `extern func main() { 0x42l; }`,
			expected: ast.NewInt64Literal(0x42),
		},
		{
			name:     "hexadecimal 64-bit large",
			src:      `extern func main() { 0xdeadbeefl; }`,
			expected: ast.NewInt64Literal(0xdeadbeef),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:      `extern func main() { ""; }`,
			expected: ast.NewStringLiteral(""),
		},
		{
			name:     "simple string",
			src:      `extern func main() { "hello"; }`,
			expected: ast.NewStringLiteral("hello"),
		},
		{
			name:     "string with spaces",
			src:      `extern func main() { "hello world"; }`,
			expected: ast.NewStringLiteral("hello world"),
		},
		{
			name:     "string with numbers",
			src:      `extern func main() { "abc123"; }`,
			expected: ast.NewStringLiteral("abc123"),
		},
		{
			name:     "string with special characters",
			src:      `extern func main() { "hello, world!"; }`,
			expected: ast.NewStringLiteral("hello, world!"),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:           `extern func main() { foo(123abc); }`,
			expectedError: "expected ',' or ')'",
		},
		{
			name:          "function call missing closing parenthesis",
			src:           `extern func main() { foo(1; }`,
			expectedError: "expected ',' or ')'",
		},
		{
			name:          "function call with invalid comma placement",
			src:           `extern func main() { foo(,1); }`,
			expectedError: "unknown expression",
		},
		{
			name:          "function call missing opening parenthesis",
			src:           `extern func main() { foo 1); }`,
			expectedError: "expected ';' after statement",
		},
		{
			name:          "empty expression",
			src:           `extern func main() { ; }`,
			expectedError: "unknown expression",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { x = 42; }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "x"},
				Value:  ast.NewIntLiteral(42),
			},
		},
		{
			name: "assignment with string literal",
			src:  `extern func main() { name = "hello"; }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "name"},
				Value:  ast.NewStringLiteral("hello"),
			},
		},
		{
			name: "assignment with function call",
			src:  `extern func main() { result = foo(); }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "result"},
				Value: &ast.FunctionCall{
					FunctionName: "foo",
					Args:         []ast.Expression{},
				},
			},
		},
		{
			name: "assignment with function call with args",
			src:  `extern func main() { result = add(1, 2); }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "result"},
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
			src:  `extern func main() { counter = 0; }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "counter"},
				Value:  ast.NewIntLiteral(0),
			},
		},
		{
			name: "assignment with empty string",
			src:  `extern func main() { text = ""; }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "text"},
				Value:  ast.NewStringLiteral(""),
			},
		},
		{
			name: "chained assignment",
			src:  `extern func main() { x = y = 1; }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "x"},
				Value: &ast.Assignment{
					Target: &ast.VariableLValue{Name: "y"},
					Value:  ast.NewIntLiteral(1),
				},
			},
		},
		{
			name: "assignment with variable",
			src:  `extern func main() { x = y; }`,
			expected: &ast.Assignment{
				Target: &ast.VariableLValue{Name: "x"},
				Value: &ast.VariableReference{
					Name: "y",
				},
			},
		},
		{
			name: "pointer dereference assignment",
			src:  `extern func main() { *p = 42; }`,
			expected: &ast.Assignment{
				Target: &ast.DereferenceLValue{
					Expression: &ast.VariableReference{Name: "p"},
				},
				Value: ast.NewIntLiteral(42),
			},
		},
		{
			name: "complex pointer dereference assignment",
			src:  `extern func main() { *(ptr + 1) = value; }`,
			expected: &ast.Assignment{
				Target: &ast.DereferenceLValue{
					Expression: &ast.BinaryOperation{
						Left:     &ast.VariableReference{Name: "ptr"},
						Operator: "+",
						Right:    ast.NewIntLiteral(1),
					},
				},
				Value: &ast.VariableReference{Name: "value"},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:           `extern func main() { x 42; }`,
			expectedError: "expected ';' after statement",
		},
		{
			name:          "assignment missing value",
			src:           `extern func main() { x = ; }`,
			expectedError: "unknown expression",
		},
		{
			name:          "assignment with invalid variable name",
			src:           `extern func main() { 123 = 42; }`,
			expectedError: "invalid assignment target",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:           `extern func main() { &foo(); }`,
			expectedError: "expected ';' after statement",
		},
		{
			name:          "address-of with literal",
			src:           `extern func main() { &42; }`,
			expectedError: "expected variable name",
		},
		{
			name:          "address-of with string literal",
			src:           `extern func main() { &"hello"; }`,
			expectedError: "expected variable name",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
		{
			name: "pointer dereference operator",
			src:  "*p",
			expected: &ast.UnaryOperation{
				Operator: "*",
				Operand:  &ast.VariableReference{Name: "p"},
			},
		},
		{
			name: "pointer dereference with parentheses",
			src:  "*(p + 1)",
			expected: &ast.UnaryOperation{
				Operator: "*",
				Operand: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "p"},
					Operator: "+",
					Right:    ast.NewIntLiteral(1),
				},
			},
		},
		{
			name: "negation of dereference",
			src:  "-*p",
			expected: &ast.UnaryOperation{
				Operator: "-",
				Operand: &ast.UnaryOperation{
					Operator: "*",
					Operand:  &ast.VariableReference{Name: "p"},
				},
			},
		},
		{
			name: "dereference of address-of",
			src:  "*&x",
			expected: &ast.UnaryOperation{
				Operator: "*",
				Operand: &ast.UnaryOperation{
					Operator: "&",
					Operand:  &ast.VariableReference{Name: "x"},
				},
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { if x == 5 { return; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { if x > 0 { y = 1; } else { y = 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
													Target: &ast.VariableLValue{Name: "y"},
													Value:  ast.NewIntLiteral(1),
												},
											},
										},
									},
									ElseBlock: &ast.Block{
										Statements: []ast.Statement{
											&ast.ExpressionStatement{
												Expression: &ast.Assignment{
													Target: &ast.VariableLValue{Name: "y"},
													Value:  ast.NewIntLiteral(0),
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
			src:  `extern func main() { if (x > 0) && (y < 10) { return x; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { if !flag { return; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { if x > 0 { if y > 0 { return 1; } } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { if { return; } }`,
		},
		{
			name: "if without opening brace",
			src:  `extern func main() { if x > 0  return; } }`,
		},
		{
			name: "if without closing brace",
			src:  `extern func main() { if x > 0 { return; }`,
		},
		{
			name: "else without opening brace",
			src:  `extern func main() { if x > 0 { return; } else  return; } }`,
		},
		{
			name: "else without closing brace",
			src:  `extern func main() { if x > 0 { return; } else { return; }`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { if x == 1 { return 1; } else if x == 2 { return 2; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { if x == 1 { return 1; } else if x == 2 { return 2; } else { return 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { if x == 1 { return 1; } else if x == 2 { return 2; } else if x == 3 { return 3; } else { return 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { if x > 10 { return 1; } else if (x > 5) && (x <= 10) { return 2; } else { return 0; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { while x > 0 { x = x - 1; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
													Target: &ast.VariableLValue{Name: "x"},
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
			src:  `extern func main() { while (x > 0) && (y < 10) { printf("loop\n"); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while !done { done = check(); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
													Target: &ast.VariableLValue{Name: "done"},
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
			src:  `extern func main() { while x > 0 { while y > 0 { y = y - 1; } x = x - 1; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
																Target: &ast.VariableLValue{Name: "y"},
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
													Target: &ast.VariableLValue{Name: "x"},
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
			src:  `extern func main() { while i < 10 { printf("i = %d\n", i); i = i + 1; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
													Target: &ast.VariableLValue{Name: "i"},
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { while { printf("loop\n"); } }`,
		},
		{
			name: "while without opening brace",
			src:  `extern func main() { while x > 0  printf("loop\n"); } }`,
		},
		{
			name: "while without closing brace",
			src:  `extern func main() { while x > 0 { printf("loop\n"); }`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { break; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while x > 0 { break; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while x > 0 { printf("before break\n"); break; printf("after break\n"); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while x > 0 { if x == 5 { break; } break; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
			src:  `extern func main() { continue; }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while x > 0 { continue; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while x > 0 { printf("before continue\n"); continue; printf("after continue\n"); } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			src:  `extern func main() { while x > 0 { if x == 5 { continue; } break; } }`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name:     "main",
						Args:     []ast.Arg{},
						External: true,
						Body: &ast.Block{
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
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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

func TestParseExpression_FieldAccess_Simple(t *testing.T) {
	src := `extern func main() { x.field; }`
	expected := &ast.FieldAccess{
		Object:    &ast.VariableReference{Name: "x"},
		FieldName: "field",
	}

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	program, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("Error parsing program: %v", err)
	}
	if len(program.Functions) != 1 {
		t.Fatalf("Expected 1 function, got %d", len(program.Functions))
	}
	if len(program.Functions[0].Body.Statements) != 1 {
		t.Fatalf("Expected 1 statement, got %d", len(program.Functions[0].Body.Statements))
	}
	stmt, ok := program.Functions[0].Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Expected ExpressionStatement, got %T", program.Functions[0].Body.Statements[0])
	}
	if !compareASTIgnoreLocation(stmt.Expression, expected) {
		t.Errorf("Expected %+v, got %+v", expected, stmt.Expression)
	}
}

func TestParseExpression_FieldAccess_Nested(t *testing.T) {
	src := `extern func main() { x.field.subfield; }`
	expected := &ast.FieldAccess{
		Object: &ast.FieldAccess{
			Object:    &ast.VariableReference{Name: "x"},
			FieldName: "field",
		},
		FieldName: "subfield",
	}

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	program, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("Error parsing program: %v", err)
	}
	if len(program.Functions) != 1 {
		t.Fatalf("Expected 1 function, got %d", len(program.Functions))
	}
	if len(program.Functions[0].Body.Statements) != 1 {
		t.Fatalf("Expected 1 statement, got %d", len(program.Functions[0].Body.Statements))
	}
	stmt, ok := program.Functions[0].Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Expected ExpressionStatement, got %T", program.Functions[0].Body.Statements[0])
	}
	if !compareASTIgnoreLocation(stmt.Expression, expected) {
		t.Errorf("Expected %+v, got %+v", expected, stmt.Expression)
	}
}

func TestParseExpression_FieldAccess_InBinaryOperation(t *testing.T) {
	src := `extern func main() { x.field + 1; }`
	expected := &ast.BinaryOperation{
		Left: &ast.FieldAccess{
			Object:    &ast.VariableReference{Name: "x"},
			FieldName: "field",
		},
		Operator: "+",
		Right:    ast.NewIntLiteral(1),
	}

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	program, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("Error parsing program: %v", err)
	}
	if len(program.Functions) != 1 {
		t.Fatalf("Expected 1 function, got %d", len(program.Functions))
	}
	if len(program.Functions[0].Body.Statements) != 1 {
		t.Fatalf("Expected 1 statement, got %d", len(program.Functions[0].Body.Statements))
	}
	stmt, ok := program.Functions[0].Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Expected ExpressionStatement, got %T", program.Functions[0].Body.Statements[0])
	}
	if !compareASTIgnoreLocation(stmt.Expression, expected) {
		t.Errorf("Expected %+v, got %+v", expected, stmt.Expression)
	}
}

func TestParseExpression_FieldAccess_Assignment(t *testing.T) {
	src := `extern func main() { x.field = 42; }`
	expected := &ast.Assignment{
		Target: &ast.FieldLValue{
			Object:    &ast.VariableReference{Name: "x"},
			FieldName: "field",
		},
		Value: ast.NewIntLiteral(42),
	}

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	program, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("Error parsing program: %v", err)
	}
	if len(program.Functions) != 1 {
		t.Fatalf("Expected 1 function, got %d", len(program.Functions))
	}
	if len(program.Functions[0].Body.Statements) != 1 {
		t.Fatalf("Expected 1 statement, got %d", len(program.Functions[0].Body.Statements))
	}
	stmt, ok := program.Functions[0].Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Expected ExpressionStatement, got %T", program.Functions[0].Body.Statements[0])
	}
	if !compareASTIgnoreLocation(stmt.Expression, expected) {
		t.Errorf("Expected %+v, got %+v", expected, stmt.Expression)
	}
}

func TestParseExpression_FieldAccess_NestedAssignment(t *testing.T) {
	src := `extern func main() { x.field.subfield = 42; }`
	expected := &ast.Assignment{
		Target: &ast.FieldLValue{
			Object: &ast.FieldLValue{
				Object:    &ast.VariableReference{Name: "x"},
				FieldName: "field",
			},
			FieldName: "subfield",
		},
		Value: ast.NewIntLiteral(42),
	}

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	program, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("Error parsing program: %v", err)
	}
	if len(program.Functions) != 1 {
		t.Fatalf("Expected 1 function, got %d", len(program.Functions))
	}
	if len(program.Functions[0].Body.Statements) != 1 {
		t.Fatalf("Expected 1 statement, got %d", len(program.Functions[0].Body.Statements))
	}
	stmt, ok := program.Functions[0].Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Expected ExpressionStatement, got %T", program.Functions[0].Body.Statements[0])
	}
	if !compareASTIgnoreLocation(stmt.Expression, expected) {
		t.Errorf("Expected %+v, got %+v", expected, stmt.Expression)
	}
}
func TestParseExpression_FieldAccess_Error_MissingFieldName(t *testing.T) {
	src := `extern func main() { x.; }`
	expectedError := "expected field name after '.'"

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	_, err := parser.ParseProgram()
	if err == nil {
		t.Fatalf("Expected error, got nil")
	}
	if !strings.Contains(err.Error(), expectedError) {
		t.Errorf("Expected error containing %q, got %q", expectedError, err.Error())
	}
}

func TestParseExpression_FieldAccess_Error_NumberAsFieldName(t *testing.T) {
	src := `extern func main() { x.123; }`
	expectedError := "expected field name after '.'"

	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	_, err := parser.ParseProgram()
	if err == nil {
		t.Fatalf("Expected error, got nil")
	}
	if !strings.Contains(err.Error(), expectedError) {
		t.Errorf("Expected error containing %q, got %q", expectedError, err.Error())
	}
}

func TestParseExpression_NewOperator(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name: "new with primitive type",
			src:  `extern func main() { new(int); }`,
			expected: &ast.NewExpression{
				TypeExpr: ast.NewBaseType("int"),
			},
		},
		{
			name: "new with pointer type",
			src:  `extern func main() { new(*int); }`,
			expected: &ast.NewExpression{
				TypeExpr: ast.NewPointerType(ast.NewBaseType("int")),
			},
		},
		{
			name: "new with custom type",
			src:  `extern func main() { new(MyStruct); }`,
			expected: &ast.NewExpression{
				TypeExpr: ast.NewBaseType("MyStruct"),
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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

func TestParseExpression_NewOperator_Error(t *testing.T) {
	testCases := []struct {
		name          string
		src           string
		expectedError string
	}{
		{
			name:          "new without parentheses",
			src:           `extern func main() { new int; }`,
			expectedError: "expected '(' after 'new'",
		},
		{
			name:          "new with missing closing parenthesis",
			src:           `extern func main() { new(int; }`,
			expectedError: "expected ')' after type in new expression",
		},
		{
			name:          "new with empty parentheses",
			src:           `extern func main() { new(); }`,
			expectedError: "expected type",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
			parser := New(lex)
			_, err := parser.ParseProgram()
			if err == nil {
				t.Fatalf("Expected error, got nil")
			}
			if !strings.Contains(err.Error(), tc.expectedError) {
				t.Errorf("Expected error containing %q, got %q", tc.expectedError, err.Error())
			}
		})
	}
}

func TestParseExpression_NullLiteral(t *testing.T) {
	src := `extern func main() { null; }`
	lex := lexer.New(strings.NewReader(src), "test.pirx")
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

	expected := ast.NewNullLiteral()
	if !compareASTIgnoreLocation(exprStmt.Expression, expected) {
		t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, expected)
	}
}

func TestParseExpression_NullAssignment(t *testing.T) {
	src := `extern func main() { var x: *int; x = null; }`
	lex := lexer.New(strings.NewReader(src), "test.pirx")
	parser := New(lex)
	prog, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("ParseProgram() error = %v", err)
	}

	// Extract the assignment expression
	if len(prog.Functions) != 1 {
		t.Fatalf("Expected 1 function, got %d", len(prog.Functions))
	}
	if len(prog.Functions[0].Body.Statements) != 2 {
		t.Fatalf("Expected 2 statements (decl + assignment), got %d", len(prog.Functions[0].Body.Statements))
	}
	stmt := prog.Functions[0].Body.Statements[1] // Second statement is the assignment
	exprStmt, ok := stmt.(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
	}
	assignment, ok := exprStmt.Expression.(*ast.Assignment)
	if !ok {
		t.Fatalf("Expected Assignment, got %+v", exprStmt.Expression)
	}

	expected := ast.NewNullLiteral()
	if !compareASTIgnoreLocation(assignment.Value, expected) {
		t.Errorf("Assignment value got = %+v, want %+v", assignment.Value, expected)
	}
}

func TestParseExpression_BooleanLiterals(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected ast.Expression
	}{
		{
			name:     "true literal",
			src:      `extern func main() { true; }`,
			expected: ast.NewBoolLiteral(true),
		},
		{
			name:     "false literal",
			src:      `extern func main() { false; }`,
			expected: ast.NewBoolLiteral(false),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			lex := lexer.New(strings.NewReader(tc.src), "test.pirx")
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
