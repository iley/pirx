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
			src:  `func main() { var x int }`,
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
			src:  `func add(a int, b int) {}`,
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
			src:  `func main() { foo(1, "two") }`,
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
			expectedError: "expected 'func'",
		},
		{
			name:          "missing function name",
			src:           `func() {}`,
			expectedError: "expected function name",
		},
		{
			name:          "missing opening parenthesis",
			src:           `func main) {}`,
			expectedError: "expected '('",
		},
		{
			name:          "missing closing parenthesis in function call",
			src:           `func main() { foo(1 }`,
			expectedError: "expected ',' or ')'",
		},
		{
			name:          "missing closing brace",
			src:           `func main() {`,
			expectedError: "unexpected EOF",
		},
		{
			name:          "incomplete var declaration",
			src:           `func main() { var x }`,
			expectedError: "expected type",
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
