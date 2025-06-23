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
					t.Errorf("token %d: expected type %s, got %s", i, expected.Type, got.Type)
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
					t.Errorf("token %d: expected type %s, got %s", i, expected.Type, got.Type)
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

func TestLexerComments(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "comment at end of line",
			input: "hello // this is a comment\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 2, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comment on its own line",
			input: "hello\n// this is a comment\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 3, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "multiple comments",
			input: "hello // comment 1\nworld // comment 2\n42",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 2, Col: 1},
				{Type: LEX_NUMBER, Str: "42", Line: 3, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "empty comment",
			input: "hello //\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 2, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comment at end of file",
			input: "hello // comment at EOF",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comment with special characters",
			input: "hello // comment with symbols !@#$%^&*()\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "world", Line: 2, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "division operator vs comment",
			input: "x / y // division vs comment",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "x", Line: 1, Col: 1},
				{Type: LEX_OPERATOR, Str: "/", Line: 1, Col: 3},
				{Type: LEX_IDENT, Str: "y", Line: 1, Col: 5},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "single slash at EOF",
			input: "hello /",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Line: 1, Col: 1},
				{Type: LEX_OPERATOR, Str: "/", Line: 1, Col: 7},
				{Type: LEX_EOF},
			},
		},
		{
			name: "comment in complex code",
			input: `func main() { // function definition
    var x = 42 // variable declaration
    // this is a standalone comment
    return x // return statement
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
				{Type: LEX_KEYWORD, Str: "return", Line: 4, Col: 5},
				{Type: LEX_IDENT, Str: "x", Line: 4, Col: 12},
				{Type: LEX_PUNCTUATION, Str: "}", Line: 5, Col: 1},
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
					t.Errorf("token %d: expected type %s, got %s", i, expected.Type, got.Type)
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

func TestLexerEscapeSequences(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "newline escape",
			input: `"hello\nworld"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\nworld", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "tab escape",
			input: `"hello\tworld"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\tworld", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "carriage return escape",
			input: `"hello\rworld"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\rworld", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "backslash escape",
			input: `"hello\\world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\\world", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "quote escape",
			input: `"hello\"world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\"world", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "single quote escape",
			input: `"hello\'world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello'world", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "multiple escapes",
			input: `"line1\nline2\tcolumn2\nline3"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "line1\nline2\tcolumn2\nline3", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "escape at end of string",
			input: `"hello world\n"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello world\n", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "escape at start of string",
			input: `"\nhello world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "\nhello world", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "consecutive escapes",
			input: `"\\n\\t\\r"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "\\n\\t\\r", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "empty string with escape",
			input: `"\n"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "\n", Line: 1, Col: 1},
				{Type: LEX_EOF},
			},
		},
		{
			name: "complex program with escaped strings",
			input: `func main() {
    var message = "Hello\nWorld\tPirx!"
    var path = "C:\\Users\\test\\file.txt"
}`,
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "func", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "main", Line: 1, Col: 6},
				{Type: LEX_PUNCTUATION, Str: "(", Line: 1, Col: 10},
				{Type: LEX_PUNCTUATION, Str: ")", Line: 1, Col: 11},
				{Type: LEX_PUNCTUATION, Str: "{", Line: 1, Col: 13},
				{Type: LEX_KEYWORD, Str: "var", Line: 2, Col: 5},
				{Type: LEX_IDENT, Str: "message", Line: 2, Col: 9},
				{Type: LEX_OPERATOR, Str: "=", Line: 2, Col: 17},
				{Type: LEX_STRING, Str: "Hello\nWorld\tPirx!", Line: 2, Col: 19},
				{Type: LEX_KEYWORD, Str: "var", Line: 3, Col: 5},
				{Type: LEX_IDENT, Str: "path", Line: 3, Col: 9},
				{Type: LEX_OPERATOR, Str: "=", Line: 3, Col: 14},
				{Type: LEX_STRING, Str: "C:\\Users\\test\\file.txt", Line: 3, Col: 16},
				{Type: LEX_PUNCTUATION, Str: "}", Line: 4, Col: 1},
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
					t.Errorf("token %d: expected type %s, got %s", i, expected.Type, got.Type)
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

func TestLexerInvalidEscapeSequences(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "unknown escape sequence",
			input: `"hello\xworld"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "helloxworld", Line: 1, Col: 1}, // \x becomes just x
				{Type: LEX_EOF},
			},
		},
		{
			name:  "backslash at end of string",
			input: `"hello\\"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\\", Line: 1, Col: 1},
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
					t.Errorf("token %d: expected type %s, got %s", i, expected.Type, got.Type)
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

func TestLexerInvalidEscapeSequenceErrors(t *testing.T) {
	tests := []struct {
		name        string
		input       string
		expectError bool
	}{
		{
			name:        "backslash at EOF",
			input:       `"hello\`,
			expectError: true, // Should get unexpected EOF
		},
		{
			name:        "unterminated string with escape",
			input:       `"hello\n`,
			expectError: true, // Should get unexpected EOF
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

func TestLexerBooleanOperators(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "equality operators",
			input: "== !=",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "==", Line: 1, Col: 1},
				{Type: LEX_OPERATOR, Str: "!=", Line: 1, Col: 4},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comparison operators",
			input: "< > <= >=",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "<", Line: 1, Col: 1},
				{Type: LEX_OPERATOR, Str: ">", Line: 1, Col: 3},
				{Type: LEX_OPERATOR, Str: "<=", Line: 1, Col: 5},
				{Type: LEX_OPERATOR, Str: ">=", Line: 1, Col: 8},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "logical operators",
			input: "&& || !",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "&&", Line: 1, Col: 1},
				{Type: LEX_OPERATOR, Str: "||", Line: 1, Col: 4},
				{Type: LEX_OPERATOR, Str: "!", Line: 1, Col: 7},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "mixed boolean operations",
			input: "x == 42 && y != 0",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "x", Line: 1, Col: 1},
				{Type: LEX_OPERATOR, Str: "==", Line: 1, Col: 3},
				{Type: LEX_NUMBER, Str: "42", Line: 1, Col: 6},
				{Type: LEX_OPERATOR, Str: "&&", Line: 1, Col: 9},
				{Type: LEX_IDENT, Str: "y", Line: 1, Col: 12},
				{Type: LEX_OPERATOR, Str: "!=", Line: 1, Col: 14},
				{Type: LEX_NUMBER, Str: "0", Line: 1, Col: 17},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "negation and comparisons",
			input: "!found || x >= limit",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "!", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "found", Line: 1, Col: 2},
				{Type: LEX_OPERATOR, Str: "||", Line: 1, Col: 8},
				{Type: LEX_IDENT, Str: "x", Line: 1, Col: 11},
				{Type: LEX_OPERATOR, Str: ">=", Line: 1, Col: 13},
				{Type: LEX_IDENT, Str: "limit", Line: 1, Col: 16},
				{Type: LEX_EOF},
			},
		},
		{
			name: "boolean operators in function",
			input: `func compare(a: int, b: int) {
    if a <= b && a != 0 {
        return a < b || a == b
    }
}`,
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "func", Line: 1, Col: 1},
				{Type: LEX_IDENT, Str: "compare", Line: 1, Col: 6},
				{Type: LEX_PUNCTUATION, Str: "(", Line: 1, Col: 13},
				{Type: LEX_IDENT, Str: "a", Line: 1, Col: 14},
				{Type: LEX_PUNCTUATION, Str: ":", Line: 1, Col: 15},
				{Type: LEX_IDENT, Str: "int", Line: 1, Col: 17},
				{Type: LEX_PUNCTUATION, Str: ",", Line: 1, Col: 20},
				{Type: LEX_IDENT, Str: "b", Line: 1, Col: 22},
				{Type: LEX_PUNCTUATION, Str: ":", Line: 1, Col: 23},
				{Type: LEX_IDENT, Str: "int", Line: 1, Col: 25},
				{Type: LEX_PUNCTUATION, Str: ")", Line: 1, Col: 28},
				{Type: LEX_PUNCTUATION, Str: "{", Line: 1, Col: 30},
				{Type: LEX_KEYWORD, Str: "if", Line: 2, Col: 5},
				{Type: LEX_IDENT, Str: "a", Line: 2, Col: 8},
				{Type: LEX_OPERATOR, Str: "<=", Line: 2, Col: 10},
				{Type: LEX_IDENT, Str: "b", Line: 2, Col: 13},
				{Type: LEX_OPERATOR, Str: "&&", Line: 2, Col: 15},
				{Type: LEX_IDENT, Str: "a", Line: 2, Col: 18},
				{Type: LEX_OPERATOR, Str: "!=", Line: 2, Col: 20},
				{Type: LEX_NUMBER, Str: "0", Line: 2, Col: 23},
				{Type: LEX_PUNCTUATION, Str: "{", Line: 2, Col: 25},
				{Type: LEX_KEYWORD, Str: "return", Line: 3, Col: 9},
				{Type: LEX_IDENT, Str: "a", Line: 3, Col: 16},
				{Type: LEX_OPERATOR, Str: "<", Line: 3, Col: 18},
				{Type: LEX_IDENT, Str: "b", Line: 3, Col: 20},
				{Type: LEX_OPERATOR, Str: "||", Line: 3, Col: 22},
				{Type: LEX_IDENT, Str: "a", Line: 3, Col: 25},
				{Type: LEX_OPERATOR, Str: "==", Line: 3, Col: 27},
				{Type: LEX_IDENT, Str: "b", Line: 3, Col: 30},
				{Type: LEX_PUNCTUATION, Str: "}", Line: 4, Col: 5},
				{Type: LEX_PUNCTUATION, Str: "}", Line: 5, Col: 1},
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
					t.Errorf("token %d: expected type %s, got %s", i, expected.Type, got.Type)
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
