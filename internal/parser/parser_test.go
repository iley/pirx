package parser

import (
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
)

func TestParseForStatement(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected *ast.ForStatement
	}{
		{
			name:  "basic for loop",
			input: "for var i = 0; i < 10; i++ { return i; }",
			expected: &ast.ForStatement{
				Init: &ast.VariableDeclaration{
					Name:        "i",
					Initializer: &ast.Literal{IntValue: func() *int32 { v := int32(0); return &v }()},
				},
				Condition: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "i"},
					Operator: "<",
					Right:    &ast.Literal{IntValue: func() *int32 { v := int32(10); return &v }()},
				},
				Update: &ast.PostfixOperator{
					Operator: "++",
					Operand:  &ast.VariableReference{Name: "i"},
				},
				Body: ast.Block{
					Statements: []ast.Statement{
						&ast.ReturnStatement{
							Value: &ast.VariableReference{Name: "i"},
						},
					},
				},
			},
		},
		{
			name:  "for loop with empty init",
			input: "for ; i < 10; i++ { break; }",
			expected: &ast.ForStatement{
				Init: nil,
				Condition: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "i"},
					Operator: "<",
					Right:    &ast.Literal{IntValue: func() *int32 { v := int32(10); return &v }()},
				},
				Update: &ast.PostfixOperator{
					Operator: "++",
					Operand:  &ast.VariableReference{Name: "i"},
				},
				Body: ast.Block{
					Statements: []ast.Statement{
						&ast.BreakStatement{},
					},
				},
			},
		},
		{
			name:  "for loop with empty condition",
			input: "for var i = 0; ; i++ { continue; }",
			expected: &ast.ForStatement{
				Init: &ast.VariableDeclaration{
					Name:        "i",
					Initializer: &ast.Literal{IntValue: func() *int32 { v := int32(0); return &v }()},
				},
				Condition: nil,
				Update: &ast.PostfixOperator{
					Operator: "++",
					Operand:  &ast.VariableReference{Name: "i"},
				},
				Body: ast.Block{
					Statements: []ast.Statement{
						&ast.ContinueStatement{},
					},
				},
			},
		},
		{
			name:  "for loop with empty update",
			input: "for var i = 0; i < 10; { i = i + 1; }",
			expected: &ast.ForStatement{
				Init: &ast.VariableDeclaration{
					Name:        "i",
					Initializer: &ast.Literal{IntValue: func() *int32 { v := int32(0); return &v }()},
				},
				Condition: &ast.BinaryOperation{
					Left:     &ast.VariableReference{Name: "i"},
					Operator: "<",
					Right:    &ast.Literal{IntValue: func() *int32 { v := int32(10); return &v }()},
				},
				Update: nil,
				Body: ast.Block{
					Statements: []ast.Statement{
						&ast.ExpressionStatement{
							Expression: &ast.Assignment{
								Target: &ast.VariableReference{Name: "i"},
								Value: &ast.BinaryOperation{
									Left:     &ast.VariableReference{Name: "i"},
									Operator: "+",
									Right:    &ast.Literal{IntValue: func() *int32 { v := int32(1); return &v }()},
								},
							},
						},
					},
				},
			},
		},
		{
			name:  "infinite for loop",
			input: "for ; ; { break; }",
			expected: &ast.ForStatement{
				Init:      nil,
				Condition: nil,
				Update:    nil,
				Body: ast.Block{
					Statements: []ast.Statement{
						&ast.BreakStatement{},
					},
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a complete function to parse
			funcInput := "func test() { " + tt.input + " }"
			l := lexer.New(strings.NewReader(funcInput), "test.pirx")
			p := New()
			err := p.Parse(l)
			if err != nil {
				t.Fatalf("Parse error: %v", err)
			}

			program := p.GetProgram()
			if len(program.Functions) != 1 {
				t.Fatalf("Expected 1 function, got %d", len(program.Functions))
			}

			fn := program.Functions[0]
			if fn.Body == nil {
				t.Fatalf("Function body is nil")
			}

			if len(fn.Body.Statements) != 1 {
				t.Fatalf("Expected 1 statement in function body, got %d", len(fn.Body.Statements))
			}

			forStmt, ok := fn.Body.Statements[0].(*ast.ForStatement)
			if !ok {
				t.Fatalf("Expected ForStatement, got %T", fn.Body.Statements[0])
			}

			// Check init statement
			if tt.expected.Init == nil {
				if forStmt.Init != nil {
					t.Errorf("Expected nil init, got %v", forStmt.Init)
				}
			} else {
				if forStmt.Init == nil {
					t.Errorf("Expected init statement, got nil")
				} else {
					expectedVarDecl, ok := tt.expected.Init.(*ast.VariableDeclaration)
					if ok {
						actualVarDecl, ok := forStmt.Init.(*ast.VariableDeclaration)
						if !ok {
							t.Errorf("Expected VariableDeclaration in init, got %T", forStmt.Init)
						} else if actualVarDecl.Name != expectedVarDecl.Name {
							t.Errorf("Expected init var name %s, got %s", expectedVarDecl.Name, actualVarDecl.Name)
						}
					}
				}
			}

			// Check condition expression
			if tt.expected.Condition == nil {
				if forStmt.Condition != nil {
					t.Errorf("Expected nil condition, got %v", forStmt.Condition)
				}
			} else {
				if forStmt.Condition == nil {
					t.Errorf("Expected condition expression, got nil")
				}
			}

			// Check update expression
			if tt.expected.Update == nil {
				if forStmt.Update != nil {
					t.Errorf("Expected nil update, got %v", forStmt.Update)
				}
			} else {
				if forStmt.Update == nil {
					t.Errorf("Expected update expression, got nil")
				}
			}

			// Check that body exists and has statements
			if len(forStmt.Body.Statements) == 0 {
				t.Errorf("Expected body with statements, got empty body")
			}
		})
	}
}

func TestParseForStatementErrors(t *testing.T) {
	tests := []struct {
		name        string
		input       string
		expectError bool
	}{
		{
			name:        "missing first semicolon",
			input:       "func test() { for var i = 0 i < 10; i++ { } }",
			expectError: true,
		},
		{
			name:        "missing second semicolon",
			input:       "func test() { for var i = 0; i < 10 i++ { } }",
			expectError: true,
		},
		{
			name:        "missing opening brace",
			input:       "func test() { for var i = 0; i < 10; i++ } }",
			expectError: true,
		},
		{
			name:        "valid minimal for loop",
			input:       "func test() { for ; ; { break; } }",
			expectError: false,
		},
		{
			name:        "valid complete for loop",
			input:       "func test() { for var i = 0; i < 10; i++ { break; } }",
			expectError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := lexer.New(strings.NewReader(tt.input), "test.pirx")
			p := New()
			err := p.Parse(l)

			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
			} else {
				if err != nil {
					t.Errorf("Expected no error but got: %v", err)
				}
			}
		})
	}
}

func TestParseForStatementString(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "basic for loop",
			input:    "func test() { for var i = 0; i < 10; i++ { break; } }",
			expected: "(for (decl i inferred 0) (< i 10) (++ i) (block (break)))",
		},
		{
			name:     "for loop with empty init",
			input:    "func test() { for ; i < 10; i++ { break; } }",
			expected: "(for  (< i 10) (++ i) (block (break)))",
		},
		{
			name:     "for loop with empty condition",
			input:    "func test() { for var i = 0; ; i++ { break; } }",
			expected: "(for (decl i inferred 0)  (++ i) (block (break)))",
		},
		{
			name:     "for loop with empty update",
			input:    "func test() { for var i = 0; i < 10; { break; } }",
			expected: "(for (decl i inferred 0) (< i 10)  (block (break)))",
		},
		{
			name:     "infinite for loop",
			input:    "func test() { for ; ; { break; } }",
			expected: "(for    (block (break)))",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := lexer.New(strings.NewReader(tt.input), "test.pirx")
			p := New()
			err := p.Parse(l)
			if err != nil {
				t.Fatalf("Parse error: %v", err)
			}

			program := p.GetProgram()
			if len(program.Functions) != 1 {
				t.Fatalf("Expected 1 function, got %d", len(program.Functions))
			}

			fn := program.Functions[0]
			if fn.Body == nil {
				t.Fatalf("Function body is nil")
			}

			if len(fn.Body.Statements) != 1 {
				t.Fatalf("Expected 1 statement in function body, got %d", len(fn.Body.Statements))
			}

			forStmt, ok := fn.Body.Statements[0].(*ast.ForStatement)
			if !ok {
				t.Fatalf("Expected ForStatement, got %T", fn.Body.Statements[0])
			}

			actual := forStmt.String()
			if actual != tt.expected {
				t.Errorf("Expected string representation %q, got %q", tt.expected, actual)
			}
		})
	}
}
