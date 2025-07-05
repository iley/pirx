package parser

import (
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
)

func TestParseProgram_ExternFunction(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *ast.Program
	}{
		{
			name: "simple extern function declaration",
			src:  `extern func atoi(x: string): int;`,
			expected: &ast.Program{
				Loc: ast.Location{Line: 1, Col: 1},
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Loc:        ast.Location{Line: 1, Col: 1},
						Name:       "atoi",
						Args:       []ast.Arg{{Name: "x", Type: "string"}},
						ReturnType: "int",
					},
				},
			},
		},
		{
			name: "extern function with no arguments",
			src:  `extern func getpid(): int;`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name:       "getpid",
						Args:       []ast.Arg{},
						ReturnType: "int",
					},
				},
			},
		},
		{
			name: "extern function with multiple arguments",
			src:  `extern func strcmp(s1: string, s2: string): int;`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "strcmp",
						Args: []ast.Arg{
							{Name: "s1", Type: "string"},
							{Name: "s2", Type: "string"},
						},
						ReturnType: "int",
					},
				},
			},
		},
		{
			name: "program with both extern and regular functions",
			src:  `extern func printf(format: string): int; func main() {}`,
			expected: &ast.Program{
				Functions: []ast.Function{
					{
						Name: "main",
						Args: []ast.Arg{},
						Body: ast.Block{Statements: []ast.Statement{}},
					},
				},
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "printf",
						Args: []ast.Arg{
							{Name: "format", Type: "string"},
						},
						ReturnType: "int",
					},
				},
			},
		},
		{
			name: "multiple extern functions",
			src:  `extern func malloc(size: int): int; extern func free(ptr: int): int;`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "malloc",
						Args: []ast.Arg{
							{Name: "size", Type: "int"},
						},
						ReturnType: "int",
					},
					{
						Name: "free",
						Args: []ast.Arg{
							{Name: "ptr", Type: "int"},
						},
						ReturnType: "int",
					},
				},
			},
		},
		{
			name: "extern function with string return type",
			src:  `extern func getenv(name: string): string;`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "getenv",
						Args: []ast.Arg{
							{Name: "name", Type: "string"},
						},
						ReturnType: "string",
					},
				},
			},
		},
		{
			name: "void extern function (no return type)",
			src:  `extern func exit(status: int);`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "exit",
						Args: []ast.Arg{
							{Name: "status", Type: "int"},
						},
						ReturnType: "",
					},
				},
			},
		},
		{
			name: "void extern function with no arguments",
			src:  `extern func abort();`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name:       "abort",
						Args:       []ast.Arg{},
						ReturnType: "",
					},
				},
			},
		},
		{
			name: "mixed void and non-void extern functions",
			src:  `extern func exit(status: int); extern func malloc(size: int): int;`,
			expected: &ast.Program{
				Functions: []ast.Function{},
				ExternFunctions: []ast.ExternFunction{
					{
						Name: "exit",
						Args: []ast.Arg{
							{Name: "status", Type: "int"},
						},
						ReturnType: "",
					},
					{
						Name: "malloc",
						Args: []ast.Arg{
							{Name: "size", Type: "int"},
						},
						ReturnType: "int",
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

func TestParseProgram_ExternFunction_Error(t *testing.T) {
	testCases := []struct {
		name          string
		src           string
		expectedError string
	}{
		{
			name:          "extern without func keyword",
			src:           `extern atoi(x: string): int;`,
			expectedError: "expected 'func' after 'extern'",
		},
		{
			name:          "extern func without name",
			src:           `extern func (x: string): int;`,
			expectedError: "expected function name",
		},
		{
			name:          "extern func without opening parenthesis",
			src:           `extern func atoi x: string): int;`,
			expectedError: "expected '('",
		},
		{
			name:          "extern func without closing parenthesis",
			src:           `extern func atoi(x: string, y`,
			expectedError: "expected ':' after parameter name",
		},
		{
			name:          "extern func without return type",
			src:           `extern func atoi(x: string):;`,
			expectedError: "expected return type",
		},
		{
			name:          "extern func missing argument type",
			src:           `extern func atoi(x): int;`,
			expectedError: "expected ':' after parameter name",
		},
		{
			name:          "extern func with invalid argument syntax",
			src:           `extern func atoi(: string): int;`,
			expectedError: "expected arg name",
		},
		{
			name:          "extern func missing semicolon",
			src:           `extern func atoi(x: string): int`,
			expectedError: "expected ';' after extern function declaration",
		},
		{
			name:          "extern void func missing semicolon",
			src:           `extern func exit(status: int)`,
			expectedError: "expected ';' after extern function declaration",
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