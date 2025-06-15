package lexer

import (
	"strings"
	"testing"
)

func TestLexer(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "empty input",
			input: "",
			expected: []Lexeme{
				{Type: LEX_EOF},
			},
		},
		{
			name:  "simple identifiers",
			input: "hello world _test123",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 1, Col: 7},
				{Type: LEX_IDENT, Str: "_test123", Line: 1, Col: 13},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "keywords",
			input: "func var val if else",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "func", Line: 1, Col: 1},
				{Type: LEX_KEYWORD, Str: "var", Line: 1, Col: 6},
				{Type: LEX_KEYWORD, Str: "val", Line: 1, Col: 10},
				{Type: LEX_KEYWORD, Str: "if", Line: 1, Col: 14},
				{Type: LEX_KEYWORD, Str: "else", Line: 1, Col: 17},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "numbers",
			input: "42 123 0",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "42", Line: 1, Col: 1},
				{Type: LEX_NUMBER, Str: "123", Line: 1, Col: 4},
				{Type: LEX_NUMBER, Str: "0", Line: 1, Col: 8},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "string literals",
			input: `"hello" "world" "with spaces"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_STRING, Str: "world", Line: 1, Col: 9},
				{Type: LEX_STRING, Str: "with spaces", Line: 1, Col: 17},
				{Type: LEX_EOF},
			},
		},
		{
			name: "mixed tokens",
			input: `func main() {
    var x = 42
    val message = "Hello, Pirx!"
}`,
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "func", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "main", Line: 1, Col: 6},
				{Type: LEX_PUNCTUATION, Str: "(", Line: 1, Col: 10},
				{Type: LEX_PUNCTUATION, Str: ")", Line: 1, Col: 11},
				{Type: LEX_PUNCTUATION, Str: "{", Line: 1, Col: 13},
				{Type: LEX_KEYWORD, Str: "var", Line: 2, Col: 5},
				{Type: LEX_IDENT, Str: "x", Line: 2, Col: 9},
				{Type: LEX_OPERATOR, Str: "=", Line: 2, Col: 11},
				{Type: LEX_NUMBER, Str: "42", Line: 2, Col: 13},
				{Type: LEX_KEYWORD, Str: "val", Line: 3, Col: 5},
				{Type: LEX_IDENT, Str: "message", Line: 3, Col: 9},
				{Type: LEX_OPERATOR, Str: "=", Line: 3, Col: 17},
				{Type: LEX_STRING, Str: "Hello, Pirx!", Line: 3, Col: 19},
				{Type: LEX_PUNCTUATION, Str: "}", Line: 4, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "multiline input",
			input: "hello\nworld\n42",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 2, Col: 1},
				{Type: LEX_NUMBER, Str: "42", Line: 3, Col: 1},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input))
			for i, expected := range tt.expected {
				got, err := l.Next()
				if err != nil {
					t.Errorf("unexpected error: %v", err)
					return
				}

				if got.Type != expected.Type {
					t.Errorf("token %d: expected type %v, got %v", i, expected.Type, got.Type)
				}
				if got.Str != expected.Str {
					t.Errorf("token %d: expected string %q, got %q", i, expected.Str, got.Str)
				}
				if got.Line != expected.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Line, got.Line)
				}
				if got.Col != expected.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Col, got.Col)
				}
			}
		})
	}
}

func TestLexerErrors(t *testing.T) {
	tests := []struct {
		name        string
		input       string
		expectError bool
	}{
		{
			name:        "unterminated string",
			input:       `"hello`,
			expectError: true,
		},
		{
			name:        "valid input",
			input:       "hello world",
			expectError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input))
			_, err := l.Next()
			if (err != nil) != tt.expectError {
				t.Errorf("expected error: %v, got: %v", tt.expectError, err)
			}
		})
	}
}

func TestLexerWhitespace(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "various whitespace",
			input: "  hello\t\n  world  \n  42",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 3},
				{Type: LEX_IDENT, Str: "world", Line: 2, Col: 3},
				{Type: LEX_NUMBER, Str: "42", Line: 3, Col: 3},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "only whitespace",
			input: "  \t\n  ",
			expected: []Lexeme{
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input))
			for i, expected := range tt.expected {
				got, err := l.Next()
				if err != nil {
					t.Errorf("unexpected error: %v", err)
					return
				}

				if got.Type != expected.Type {
					t.Errorf("token %d: expected type %v, got %v", i, expected.Type, got.Type)
				}
				if got.Str != expected.Str {
					t.Errorf("token %d: expected string %q, got %q", i, expected.Str, got.Str)
				}
				if got.Line != expected.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Line, got.Line)
				}
				if got.Col != expected.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Col, got.Col)
				}
			}
		})
	}
}

func lexAll(l *Lexer) ([]Lexeme, error) {
	lexemes := []Lexeme{}
	for {
		lex, err := l.Next()
		if err != nil {
			return nil, err
		}
		lexemes = append(lexemes, lex)
		if lex.Type == LEX_EOF {
			break
		}
	}
	return lexemes, nil
}
