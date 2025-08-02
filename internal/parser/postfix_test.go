package parser

import (
	"strings"
	"testing"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
)

func TestPostfixIncrementParsing(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "simple variable increment",
			input:    "x++",
			expected: "(++ x)",
		},
		{
			name:     "field access increment",
			input:    "obj.count++",
			expected: "(++ (. obj count))",
		},
		{
			name:     "array element increment",
			input:    "arr[i]++",
			expected: "(++ ([] arr i))",
		},
		{
			name:     "increment with addition",
			input:    "x++ + y",
			expected: "(+ (++ x) y)",
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
				t.Fatalf("Failed to parse postfix increment %q: %v", tt.input, err)
			}

			if expr.String() != tt.expected {
				t.Errorf("Expected AST %q, got %q", tt.expected, expr.String())
			}

			// Verify the top-level expression type
			switch tt.name {
			case "simple variable increment", "field access increment", "array element increment":
				if postfixOp, ok := expr.(*ast.PostfixOperator); !ok {
					t.Errorf("Expected *ast.PostfixOperator, got %T", expr)
				} else if postfixOp.Operator != "++" {
					t.Errorf("Expected operator '++', got '%s'", postfixOp.Operator)
				}
			case "increment with addition":
				if binOp, ok := expr.(*ast.BinaryOperation); ok {
					if postfixOp, ok := binOp.Left.(*ast.PostfixOperator); !ok {
						t.Errorf("Expected left operand to be *ast.PostfixOperator, got %T", binOp.Left)
					} else if postfixOp.Operator != "++" {
						t.Errorf("Expected operator '++', got '%s'", postfixOp.Operator)
					}
				} else {
					t.Errorf("Expected *ast.BinaryOperation, got %T", expr)
				}
			}
		})
	}
}

func TestPostfixIncrementInvalidCases(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "increment of literal",
			input: "42++",
		},
		{
			name:  "increment of function call",
			input: "getValue()++",
		},
		{
			name:  "increment of expression",
			input: "(x + y)++",
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
