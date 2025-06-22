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
								{VariableDeclaration: &VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
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
								{ExpressionStatement: &ExpressionStatement{
									Expression: Expression{FunctionCall: &FunctionCall{
										FunctionName: "foo",
										Args: []Expression{
											{Literal: NewIntLiteral(1)},
											{Literal: NewStringLiteral("two")},
										},
									}},
								}},
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
								{VariableDeclaration: &VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
								{VariableDeclaration: &VariableDeclaration{
									Name: "y",
									Type: "string",
								}},
							},
						},
					},
				},
			},
		},
		{
			name: "program with two empty functions",
			src:  `func main() {} func helper() {}`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body:   &Block{Statements: []Statement{}},
					},
					{
						Name:   "helper",
						Params: []*Param{},
						Body:   &Block{Statements: []Statement{}},
					},
				},
			},
		},
		{
			name: "function with return without value",
			src:  `func main() { return; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								{ReturnStatement: &ReturnStatement{
									Value: nil,
								}},
							},
						},
					},
				},
			},
		},
		{
			name: "function with return with integer value",
			src:  `func main() { return 42; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								{ReturnStatement: &ReturnStatement{
									Value: &Expression{Literal: NewIntLiteral(42)},
								}},
							},
						},
					},
				},
			},
		},
		{
			name: "function with return with string value",
			src:  `func main() { return "hello"; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								{ReturnStatement: &ReturnStatement{
									Value: &Expression{Literal: NewStringLiteral("hello")},
								}},
							},
						},
					},
				},
			},
		},
		{
			name: "function with return with function call",
			src:  `func main() { return foo(); }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								{ReturnStatement: &ReturnStatement{
									Value: &Expression{FunctionCall: &FunctionCall{
										FunctionName: "foo",
										Args:         []Expression{},
									}},
								}},
							},
						},
					},
				},
			},
		},
		{
			name: "function with multiple return statements",
			src:  `func main() { return 1; return "two"; return; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								{ReturnStatement: &ReturnStatement{
									Value: &Expression{Literal: NewIntLiteral(1)},
								}},
								{ReturnStatement: &ReturnStatement{
									Value: &Expression{Literal: NewStringLiteral("two")},
								}},
								{ReturnStatement: &ReturnStatement{
									Value: nil,
								}},
							},
						},
					},
				},
			},
		},
		{
			name: "function with mixed statements including return",
			src:  `func main() { var x: int; return x; }`,
			expected: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								{VariableDeclaration: &VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
								{ReturnStatement: &ReturnStatement{
									Value: &Expression{VariableReference: &VariableReference{
										Name: "x",
									}},
								}},
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
			expected: Expression{FunctionCall: &FunctionCall{
				FunctionName: "foo",
				Args:         []Expression{},
			}},
		},
		{
			name: "function call with single integer argument",
			src:  `func main() { foo(42); }`,
			expected: Expression{FunctionCall: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					{Literal: NewIntLiteral(42)},
				},
			}},
		},
		{
			name: "function call with single string argument",
			src:  `func main() { foo("hello"); }`,
			expected: Expression{FunctionCall: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					{Literal: NewStringLiteral("hello")},
				},
			}},
		},
		{
			name: "function call with multiple arguments",
			src:  `func main() { foo(1, "two", 3); }`,
			expected: Expression{FunctionCall: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					{Literal: NewIntLiteral(1)},
					{Literal: NewStringLiteral("two")},
					{Literal: NewIntLiteral(3)},
				},
			}},
		},
		{
			name: "nested function calls",
			src:  `func main() { foo(bar()); }`,
			expected: Expression{FunctionCall: &FunctionCall{
				FunctionName: "foo",
				Args: []Expression{
					{FunctionCall: &FunctionCall{
						FunctionName: "bar",
						Args:         []Expression{},
					}},
				},
			}},
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
			if stmt.ExpressionStatement == nil {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !reflect.DeepEqual(stmt.ExpressionStatement.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", stmt.ExpressionStatement.Expression, tc.expected)
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
			name:     "zero",
			src:      `func main() { 0; }`,
			expected: Expression{Literal: NewIntLiteral(0)},
		},
		{
			name:     "positive integer",
			src:      `func main() { 42; }`,
			expected: Expression{Literal: NewIntLiteral(42)},
		},
		{
			name:     "large integer",
			src:      `func main() { 999999; }`,
			expected: Expression{Literal: NewIntLiteral(999999)},
		},
		{
			name:     "single digit",
			src:      `func main() { 7; }`,
			expected: Expression{Literal: NewIntLiteral(7)},
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
			if stmt.ExpressionStatement == nil {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !reflect.DeepEqual(stmt.ExpressionStatement.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", stmt.ExpressionStatement.Expression, tc.expected)
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
			name:     "empty string",
			src:      `func main() { ""; }`,
			expected: Expression{Literal: NewStringLiteral("")},
		},
		{
			name:     "simple string",
			src:      `func main() { "hello"; }`,
			expected: Expression{Literal: NewStringLiteral("hello")},
		},
		{
			name:     "string with spaces",
			src:      `func main() { "hello world"; }`,
			expected: Expression{Literal: NewStringLiteral("hello world")},
		},
		{
			name:     "string with numbers",
			src:      `func main() { "abc123"; }`,
			expected: Expression{Literal: NewStringLiteral("abc123")},
		},
		{
			name:     "string with special characters",
			src:      `func main() { "hello, world!"; }`,
			expected: Expression{Literal: NewStringLiteral("hello, world!")},
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
			if stmt.ExpressionStatement == nil {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !reflect.DeepEqual(stmt.ExpressionStatement.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", stmt.ExpressionStatement.Expression, tc.expected)
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
		expected Expression
	}{
		{
			name: "assignment with integer literal",
			src:  `func main() { x = 42; }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "x",
				Value:        Expression{Literal: NewIntLiteral(42)},
			}},
		},
		{
			name: "assignment with string literal",
			src:  `func main() { name = "hello"; }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "name",
				Value:        Expression{Literal: NewStringLiteral("hello")},
			}},
		},
		{
			name: "assignment with function call",
			src:  `func main() { result = foo(); }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "result",
				Value: Expression{FunctionCall: &FunctionCall{
					FunctionName: "foo",
					Args:         []Expression{},
				}},
			}},
		},
		{
			name: "assignment with function call with args",
			src:  `func main() { result = add(1, 2); }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "result",
				Value: Expression{FunctionCall: &FunctionCall{
					FunctionName: "add",
					Args: []Expression{
						{Literal: NewIntLiteral(1)},
						{Literal: NewIntLiteral(2)},
					},
				}},
			}},
		},
		{
			name: "assignment with zero",
			src:  `func main() { counter = 0; }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "counter",
				Value:        Expression{Literal: NewIntLiteral(0)},
			}},
		},
		{
			name: "assignment with empty string",
			src:  `func main() { text = ""; }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "text",
				Value:        Expression{Literal: NewStringLiteral("")},
			}},
		},
		{
			name: "chained assignment",
			src:  `func main() { x = y = 1; }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "x",
				Value: Expression{Assignment: &Assignment{
					VariableName: "y",
					Value:        Expression{Literal: NewIntLiteral(1)},
				}},
			}},
		},
		{
			name: "assignment with variable",
			src:  `func main() { x = y; }`,
			expected: Expression{Assignment: &Assignment{
				VariableName: "x",
				Value: Expression{VariableReference: &VariableReference{
					Name: "y",
				}},
			}},
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
			if stmt.ExpressionStatement == nil {
				t.Fatalf("Expected ExpressionStatement, got %+v", stmt)
			}

			if !reflect.DeepEqual(stmt.ExpressionStatement.Expression, tc.expected) {
				t.Errorf("Expression got = %+v, want %+v", stmt.ExpressionStatement.Expression, tc.expected)
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

func TestParseExpression_BinaryOperation(t *testing.T) {
	testCases := []struct {
		name     string
		src      string
		expected Expression
	}{
		{
			name: "simple addition",
			src:  "1 + 2",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(1)},
				Operator: "+",
				Right:    Expression{Literal: NewIntLiteral(2)},
			}},
		},
		{
			name: "addition with variables",
			src:  "x + y",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "x"}},
				Operator: "+",
				Right:    Expression{VariableReference: &VariableReference{Name: "y"}},
			}},
		},
		{
			name: "addition with mixed types",
			src:  "5 + x",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(5)},
				Operator: "+",
				Right:    Expression{VariableReference: &VariableReference{Name: "x"}},
			}},
		},
		{
			name: "simple subtraction",
			src:  "10 - 3",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(10)},
				Operator: "-",
				Right:    Expression{Literal: NewIntLiteral(3)},
			}},
		},
		{
			name: "subtraction with variables",
			src:  "x - y",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "x"}},
				Operator: "-",
				Right:    Expression{VariableReference: &VariableReference{Name: "y"}},
			}},
		},
		{
			name: "subtraction with mixed types",
			src:  "15 - z",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(15)},
				Operator: "-",
				Right:    Expression{VariableReference: &VariableReference{Name: "z"}},
			}},
		},
		{
			name: "variable subtraction from literal",
			src:  "a - 5",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "a"}},
				Operator: "-",
				Right:    Expression{Literal: NewIntLiteral(5)},
			}},
		},
		{
			name: "simple multiplication",
			src:  "3 * 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(3)},
				Operator: "*",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "multiplication with variables",
			src:  "x * y",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "x"}},
				Operator: "*",
				Right:    Expression{VariableReference: &VariableReference{Name: "y"}},
			}},
		},
		{
			name: "multiplication with mixed types",
			src:  "7 * z",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(7)},
				Operator: "*",
				Right:    Expression{VariableReference: &VariableReference{Name: "z"}},
			}},
		},
		{
			name: "variable multiplication with literal",
			src:  "b * 8",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "b"}},
				Operator: "*",
				Right:    Expression{Literal: NewIntLiteral(8)},
			}},
		},
		{
			name: "mixed addition and multiplication (correct precedence)",
			src:  "2 + 3 * 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(2)},
				Operator: "+",
				Right: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(3)},
					Operator: "*",
					Right:    Expression{Literal: NewIntLiteral(4)},
				}},
			}},
		},
		{
			name: "mixed multiplication and addition (correct precedence)",
			src:  "2 * 3 + 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(2)},
					Operator: "*",
					Right:    Expression{Literal: NewIntLiteral(3)},
				}},
				Operator: "+",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "multiple multiplications (left associative)",
			src:  "2 * 3 * 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(2)},
					Operator: "*",
					Right:    Expression{Literal: NewIntLiteral(3)},
				}},
				Operator: "*",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "complex precedence test",
			src:  "1 + 2 * 3 + 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(1)},
					Operator: "+",
					Right: Expression{BinaryOperation: &BinaryOperation{
						Left:     Expression{Literal: NewIntLiteral(2)},
						Operator: "*",
						Right:    Expression{Literal: NewIntLiteral(3)},
					}},
				}},
				Operator: "+",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "subtraction and multiplication precedence",
			src:  "10 - 2 * 3",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(10)},
				Operator: "-",
				Right: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(2)},
					Operator: "*",
					Right:    Expression{Literal: NewIntLiteral(3)},
				}},
			}},
		},
		{
			name: "simple division",
			src:  "12 / 3",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(12)},
				Operator: "/",
				Right:    Expression{Literal: NewIntLiteral(3)},
			}},
		},
		{
			name: "division with variables",
			src:  "x / y",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "x"}},
				Operator: "/",
				Right:    Expression{VariableReference: &VariableReference{Name: "y"}},
			}},
		},
		{
			name: "division with mixed types",
			src:  "20 / z",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(20)},
				Operator: "/",
				Right:    Expression{VariableReference: &VariableReference{Name: "z"}},
			}},
		},
		{
			name: "variable division with literal",
			src:  "w / 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{VariableReference: &VariableReference{Name: "w"}},
				Operator: "/",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "mixed addition and division (correct precedence)",
			src:  "2 + 8 / 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left:     Expression{Literal: NewIntLiteral(2)},
				Operator: "+",
				Right: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(8)},
					Operator: "/",
					Right:    Expression{Literal: NewIntLiteral(4)},
				}},
			}},
		},
		{
			name: "mixed division and addition (correct precedence)",
			src:  "12 / 3 + 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(12)},
					Operator: "/",
					Right:    Expression{Literal: NewIntLiteral(3)},
				}},
				Operator: "+",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "multiplication and division (same precedence, left associative)",
			src:  "8 * 2 / 4",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(8)},
					Operator: "*",
					Right:    Expression{Literal: NewIntLiteral(2)},
				}},
				Operator: "/",
				Right:    Expression{Literal: NewIntLiteral(4)},
			}},
		},
		{
			name: "division and multiplication (same precedence, left associative)",
			src:  "12 / 3 * 2",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(12)},
					Operator: "/",
					Right:    Expression{Literal: NewIntLiteral(3)},
				}},
				Operator: "*",
				Right:    Expression{Literal: NewIntLiteral(2)},
			}},
		},
		{
			name: "multiple divisions (left associative)",
			src:  "24 / 4 / 2",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(24)},
					Operator: "/",
					Right:    Expression{Literal: NewIntLiteral(4)},
				}},
				Operator: "/",
				Right:    Expression{Literal: NewIntLiteral(2)},
			}},
		},
		{
			name: "complex expression with division",
			src:  "1 + 12 / 3 - 2",
			expected: Expression{BinaryOperation: &BinaryOperation{
				Left: Expression{BinaryOperation: &BinaryOperation{
					Left:     Expression{Literal: NewIntLiteral(1)},
					Operator: "+",
					Right: Expression{BinaryOperation: &BinaryOperation{
						Left:     Expression{Literal: NewIntLiteral(12)},
						Operator: "/",
						Right:    Expression{Literal: NewIntLiteral(3)},
					}},
				}},
				Operator: "-",
				Right:    Expression{Literal: NewIntLiteral(2)},
			}},
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
			if !reflect.DeepEqual(result, tc.expected) {
				t.Errorf("expected %+v, got %+v", tc.expected, result)
			}
		})
	}
}
