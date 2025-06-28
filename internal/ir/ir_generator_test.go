package ir

import (
	"reflect"
	"testing"

	"github.com/iley/pirx/internal/parser"
)

func TestIrGenerator_ReturnStatements(t *testing.T) {
	testCases := []struct {
		name     string
		program  *parser.Program
		expected IrProgram
	}{
		{
			name: "function with bare return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ReturnStatement: &parser.ReturnStatement{
									Value: nil,
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Return{Value: nil},
						},
					},
				},
			},
		},
		{
			name: "function with return integer literal",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{Literal: parser.NewIntLiteral(42)},
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Return{Value: &Arg{LiteralInt: intPtr(42)}},
						},
					},
				},
			},
		},
		{
			name: "function with return variable",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{VariableReference: &parser.VariableReference{
										Name: "x",
									}},
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Return{Value: &Arg{Variable: "x"}},
						},
					},
				},
			},
		},
		{
			name: "function with return function call",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{FunctionCall: &parser.FunctionCall{
										FunctionName: "foo",
										Args:         []parser.Expression{},
									}},
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Call{Result: "$1", Function: "foo", Args: []Arg{}},
							Return{Value: &Arg{Variable: "$1"}},
						},
					},
				},
			},
		},
		{
			name: "function with mixed statements including return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{VariableDeclaration: &parser.VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{VariableReference: &parser.VariableReference{
										Name: "x",
									}},
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Assign{Target: "x", Value: Arg{LiteralInt: intPtr(0)}},
							Return{Value: &Arg{Variable: "x"}},
						},
					},
				},
			},
		},
		{
			name: "function with multiple return statements",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{Literal: parser.NewIntLiteral(1)},
								}},
								{ReturnStatement: &parser.ReturnStatement{
									Value: nil,
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Return{Value: &Arg{LiteralInt: intPtr(1)}},
							Return{Value: nil},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result := Generate(tc.program)

			if !reflect.DeepEqual(result, tc.expected) {
				t.Errorf("Generate() got = %+v, want %+v", result, tc.expected)
			}
		})
	}
}

// Helper function to create int pointers
func intPtr(i int64) *int64 {
	return &i
}

func TestIrGenerator_ImplicitReturn(t *testing.T) {
	testCases := []struct {
		name     string
		program  *parser.Program
		expected IrProgram
	}{
		{
			name: "empty function gets implicit return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body:   &parser.Block{Statements: []parser.Statement{}},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Return{Value: nil},
						},
					},
				},
			},
		},
		{
			name: "function with variable declaration gets implicit return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{VariableDeclaration: &parser.VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Assign{Target: "x", Value: Arg{LiteralInt: intPtr(0)}},
							Return{Value: nil},
						},
					},
				},
			},
		},
		{
			name: "function with expression statement gets implicit return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ExpressionStatement: &parser.ExpressionStatement{
									Expression: parser.Expression{FunctionCall: &parser.FunctionCall{
										FunctionName: "foo",
										Args:         []parser.Expression{},
									}},
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Call{Result: "$1", Function: "foo", Args: []Arg{}},
							Return{Value: nil},
						},
					},
				},
			},
		},
		{
			name: "function ending with explicit return does not get implicit return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{VariableDeclaration: &parser.VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{Literal: parser.NewIntLiteral(42)},
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Assign{Target: "x", Value: Arg{LiteralInt: intPtr(0)}},
							Return{Value: &Arg{LiteralInt: intPtr(42)}},
						},
					},
				},
			},
		},
		{
			name: "function with multiple statements ending with non-return gets implicit return",
			program: &parser.Program{
				Functions: []*parser.Function{
					{
						Name:   "test",
						Params: []*parser.Param{},
						Body: &parser.Block{
							Statements: []parser.Statement{
								{ReturnStatement: &parser.ReturnStatement{
									Value: &parser.Expression{Literal: parser.NewIntLiteral(1)},
								}},
								{VariableDeclaration: &parser.VariableDeclaration{
									Name: "x",
									Type: "int",
								}},
							},
						},
					},
				},
			},
			expected: IrProgram{
				Functions: []IrFunction{
					{
						Name:   "test",
						Params: []string{},
						Ops: []Op{
							Return{Value: &Arg{LiteralInt: intPtr(1)}},
							Assign{Target: "x", Value: Arg{LiteralInt: intPtr(0)}},
							Return{Value: nil},
						},
					},
				},
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result := Generate(tc.program)

			if !reflect.DeepEqual(result, tc.expected) {
				t.Errorf("Generate() got = %+v, want %+v", result, tc.expected)
			}
		})
	}
}
