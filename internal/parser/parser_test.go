package parser

import (
	"reflect"
	"strings"
	"testing"

	"github.com/iley/pirx/internal/lexer"
)

func TestParseProgram(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected *Program
	}{
		{
			name: "trivial program",
			src:  `func main() {}`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body:   &Block{Statements: []Statement{}},
					},
				},
			},
		},
		{
			name: "function with var declaration",
			src:  `func main() { var x: int; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								&VariableDeclaration{
									Name: "x",
									Type: "int",
								},
							},
						},
					},
				},
			},
		},
		{
			name: "function with arguments",
			src:  `func add(a: int, b: int) {}`,
			expected: &Program{
				Functions: []*Function{
					{
						Name: "add",
						Params: []*Param{
							{Name: "a", Type: "int"},
							{Name: "b", Type: "int"},
						},
						Body: &Block{Statements: []Statement{}},
					},
				},
			},
		},
		{
			name: "function with expression statements",
			src:  `func main() { foo(1, "two"); }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								&ExpressionStatement{
									Expression: &FunctionCall{
										FunctionName: "foo",
										Args: []Expression{
											&Literal{
												Type:     LiteralTypeInt,
												IntValue: 1,
											},
											&Literal{
												Type:        LiteralTypeString,
												StringValue: "two",
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
			name: "function with multiple statements",
			src:  `func main() { var x: int; var y: string; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								&VariableDeclaration{
									Name: "x",
									Type: "int",
								},
								&VariableDeclaration{
									Name: "y",
									Type: "string",
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
			prog, err := parser.ParseProgram()
			if err != nil {
				t.Fatalf("ParseProgram() error = %v", err)
			}
			if !reflect.DeepEqual(prog, tc.expected) {
				t.Errorf("ParseProgram() got = %+v, want %+v", prog, tc.expected)
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
			expectedError: "1:1: expected 'func'",
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
		expected Expression
	}{
		{
			name: "function call with no arguments",
			src:  `func main() { foo(); }`,
			expected: &FunctionCall{
				FunctionName: "foo",
				Args:         []Expression{},
			},
		},
		{
			name: "function call with single integer argument",
			src:  `func main() { foo(42); }`,
			expected: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					&Literal{
						Type:     LiteralTypeInt,
						IntValue: 42,
					},
				},
			},
		},
		{
			name: "function call with single string argument",
			src:  `func main() { foo("hello"); }`,
			expected: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					&Literal{
						Type:        LiteralTypeString,
						StringValue: "hello",
					},
				},
			},
		},
		{
			name: "function call with multiple arguments",
			src:  `func main() { foo(1, "two", 3); }`,
			expected: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					&Literal{
						Type:     LiteralTypeInt,
						IntValue: 1,
					},
					&Literal{
						Type:        LiteralTypeString,
						StringValue: "two",
					},
					&Literal{
						Type:     LiteralTypeInt,
						IntValue: 3,
					},
				},
			},
		},
		{
			name: "nested function calls",
			src:  `func main() { foo(bar()); }`,
			expected: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					&FunctionCall{
						FunctionName: "bar",
						Args:         []Expression{},
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
			exprStmt, ok := prog.Functions[0].Body.Statements[0].(*ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %T", prog.Functions[0].Body.Statements[0])
			}

			if !reflect.DeepEqual(exprStmt.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, tc.expected)
			}
		})
	}
}

func TestParseExpression_IntegerLiteral(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected Expression
	}{
		{
			name: "zero",
			src:  `func main() { 0; }`,
			expected: &Literal{
				Type:     LiteralTypeInt,
				IntValue: 0,
			},
		},
		{
			name: "positive integer",
			src:  `func main() { 42; }`,
			expected: &Literal{
				Type:     LiteralTypeInt,
				IntValue: 42,
			},
		},
		{
			name: "large integer",
			src:  `func main() { 999999; }`,
			expected: &Literal{
				Type:     LiteralTypeInt,
				IntValue: 999999,
			},
		},
		{
			name: "single digit",
			src:  `func main() { 7; }`,
			expected: &Literal{
				Type:     LiteralTypeInt,
				IntValue: 7,
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
			exprStmt, ok := prog.Functions[0].Body.Statements[0].(*ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %T", prog.Functions[0].Body.Statements[0])
			}

			if !reflect.DeepEqual(exprStmt.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", exprStmt.Expression, tc.expected)
			}
		})
	}
}

func TestParseExpression_StringLiteral(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected Expression
	}{
		{
			name: "empty string",
			src:  `func main() { ""; }`,
			expected: &Literal{
				Type:        LiteralTypeString,
				StringValue: "",
			},
		},
		{
			name: "simple string",
			src:  `func main() { "hello"; }`,
			expected: &Literal{
				Type:        LiteralTypeString,
				StringValue: "hello",
			},
		},
		{
			name: "string with spaces",
			src:  `func main() { "hello world"; }`,
			expected: &Literal{
				Type:        LiteralTypeString,
				StringValue: "hello world",
			},
		},
		{
			name: "string with numbers",
			src:  `func main() { "abc123"; }`,
			expected: &Literal{
				Type:        LiteralTypeString,
				StringValue: "abc123",
			},
		},
		{
			name: "string with special characters",
			src:  `func main() { "hello, world!"; }`,
			expected: &Literal{
				Type:        LiteralTypeString,
				StringValue: "hello, world!",
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
			exprStmt, ok := prog.Functions[0].Body.Statements[0].(*ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %T", prog.Functions[0].Body.Statements[0])
			}

			if !reflect.DeepEqual(exprStmt.Expression, tc.expected) {
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
			expectedError: "expected '('",
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
		expected Expression
	}{
		{
			name: "assignment with integer literal",
			src:  `func main() { x = 42; }`,
			expected: &Assignment{
				VariableName: "x",
				Value: &Literal{
					Type:     LiteralTypeInt,
					IntValue: 42,
				},
			},
		},
		{
			name: "assignment with string literal",
			src:  `func main() { name = "hello"; }`,
			expected: &Assignment{
				VariableName: "name",
				Value: &Literal{
					Type:        LiteralTypeString,
					StringValue: "hello",
				},
			},
		},
		{
			name: "assignment with function call",
			src:  `func main() { result = foo(); }`,
			expected: &Assignment{
				VariableName: "result",
				Value: &FunctionCall{
					FunctionName: "foo",
					Args:         []Expression{},
				},
			},
		},
		{
			name: "assignment with function call with args",
			src:  `func main() { result = add(1, 2); }`,
			expected: &Assignment{
				VariableName: "result",
				Value: &FunctionCall{
					FunctionName: "add",
					Args: []Expression{
						&Literal{
							Type:     LiteralTypeInt,
							IntValue: 1,
						},
						&Literal{
							Type:     LiteralTypeInt,
							IntValue: 2,
						},
					},
				},
			},
		},
		{
			name: "assignment with zero",
			src:  `func main() { counter = 0; }`,
			expected: &Assignment{
				VariableName: "counter",
				Value: &Literal{
					Type:     LiteralTypeInt,
					IntValue: 0,
				},
			},
		},
		{
			name: "assignment with empty string",
			src:  `func main() { text = ""; }`,
			expected: &Assignment{
				VariableName: "text",
				Value: &Literal{
					Type:        LiteralTypeString,
					StringValue: "",
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
			exprStmt, ok := prog.Functions[0].Body.Statements[0].(*ExpressionStatement)
			if !ok {
				t.Fatalf("Expected ExpressionStatement, got %T", prog.Functions[0].Body.Statements[0])
			}

			if !reflect.DeepEqual(exprStmt.Expression, tc.expected) {
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
			expectedError: "expected '('",
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
