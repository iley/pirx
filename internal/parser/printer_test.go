package parser

import (
	"strings"
	"testing"
)

func TestPrinter(t *testing.T) {
	tests := []struct {
		name     string
		program  *Program
		expected string
	}{
		{
			name: "empty program",
			program: &Program{
				Functions: []*Function{},
			},
			expected: "",
		},
		{
			name: "function with no params and empty body",
			program: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{},
						},
					},
				},
			},
			expected: "func main() {\n}\n",
		},
		{
			name: "function with params",
			program: &Program{
				Functions: []*Function{
					{
						Name: "add",
						Params: []*Param{
							{Name: "a", Type: "int"},
							{Name: "b", Type: "int"},
						},
						Body: &Block{
							Statements: []Statement{},
						},
					},
				},
			},
			expected: "func add(a int, b int) {\n}\n",
		},
		{
			name: "function with variable declarations",
			program: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								&VariableDeclaration{Name: "x", Type: "int"},
								&VariableDeclaration{Name: "message", Type: "string"},
							},
						},
					},
				},
			},
			expected: "func main() {\n  var x int\n  var message string\n}\n",
		},
		{
			name: "function with literals and function calls",
			program: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								&FunctionCall{
									FunctionName: "print",
									Args: []Expression{
										&Literal{
											Type:        LiteralTypeString,
											StringValue: "Hello, Pirx!",
										},
									},
								},
								&FunctionCall{
									FunctionName: "add",
									Args: []Expression{
										&Literal{Type: LiteralTypeInt, IntValue: 42},
										&Literal{Type: LiteralTypeInt, IntValue: 58},
									},
								},
							},
						},
					},
				},
			},
			expected: "func main() {\n  print(\"Hello, Pirx!\")\n  add(42, 58)\n}\n",
		},
		{
			name: "multiple functions",
			program: &Program{
				Functions: []*Function{
					{
						Name:   "main",
						Params: []*Param{},
						Body: &Block{
							Statements: []Statement{
								&FunctionCall{
									FunctionName: "greet",
									Args: []Expression{
										&Literal{
											Type:        LiteralTypeString,
											StringValue: "World",
										},
									},
								},
							},
						},
					},
					{
						Name: "greet",
						Params: []*Param{
							{Name: "name", Type: "string"},
						},
						Body: &Block{
							Statements: []Statement{
								&FunctionCall{
									FunctionName: "print",
									Args: []Expression{
										&Literal{
											Type:        LiteralTypeString,
											StringValue: "Hello, ",
										},
									},
								},
							},
						},
					},
				},
			},
			expected: "func main() {\n  greet(\"World\")\n}\n\nfunc greet(name string) {\n  print(\"Hello, \")\n}\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf strings.Builder
			printer := NewPrinter(&buf)
			tt.program.Accept(printer)
			got := buf.String()
			if got != tt.expected {
				t.Errorf("printer output = %q, want %q", got, tt.expected)
			}
		})
	}
}
