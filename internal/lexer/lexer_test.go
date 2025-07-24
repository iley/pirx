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
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 1, Col: 7}},
				{Type: LEX_IDENT, Str: "_test123", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "keywords",
			input: "func var val if else",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "func", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_KEYWORD, Str: "val", Loc: Location{Filename: "test.pirx", Line: 1, Col: 10}},
				{Type: LEX_KEYWORD, Str: "if", Loc: Location{Filename: "test.pirx", Line: 1, Col: 14}},
				{Type: LEX_KEYWORD, Str: "else", Loc: Location{Filename: "test.pirx", Line: 1, Col: 17}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "struct keyword",
			input: "struct",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "struct", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "struct declaration",
			input: "struct Foo { x: int; y: string; }",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "struct", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "Foo", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 1, Col: 12}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 14}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 15}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 17}},
				{Type: LEX_PUNCTUATION, Str: ";", Loc: Location{Filename: "test.pirx", Line: 1, Col: 20}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 22}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 23}},
				{Type: LEX_IDENT, Str: "string", Loc: Location{Filename: "test.pirx", Line: 1, Col: 25}},
				{Type: LEX_PUNCTUATION, Str: ";", Loc: Location{Filename: "test.pirx", Line: 1, Col: 31}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 1, Col: 33}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "numbers",
			input: "42 123 0",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "123", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_NUMBER, Str: "0", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "hexadecimal numbers",
			input: "0x42 0xdeadbeef 0X123ABC",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "0x42", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "0xdeadbeef", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_NUMBER, Str: "0X123ABC", Loc: Location{Filename: "test.pirx", Line: 1, Col: 17}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "hexadecimal numbers with l suffix",
			input: "0x42l 0xdeadbeefl",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "0x42l", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "0xdeadbeefl", Loc: Location{Filename: "test.pirx", Line: 1, Col: 7}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "mixed decimal and hex numbers",
			input: "42 0x2a 123 0xdead",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "0x2a", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_NUMBER, Str: "123", Loc: Location{Filename: "test.pirx", Line: 1, Col: 9}},
				{Type: LEX_NUMBER, Str: "0xdead", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "hex numbers with various cases",
			input: "0xABCD 0xabcd 0XaBcD 0x0123456789",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "0xABCD", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "0xabcd", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_NUMBER, Str: "0XaBcD", Loc: Location{Filename: "test.pirx", Line: 1, Col: 15}},
				{Type: LEX_NUMBER, Str: "0x0123456789", Loc: Location{Filename: "test.pirx", Line: 1, Col: 22}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "edge cases with zero",
			input: "0 0x0 0x00 0xl",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "0", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "0x0", Loc: Location{Filename: "test.pirx", Line: 1, Col: 3}},
				{Type: LEX_NUMBER, Str: "0x00", Loc: Location{Filename: "test.pirx", Line: 1, Col: 7}},
				{Type: LEX_NUMBER, Str: "0xl", Loc: Location{Filename: "test.pirx", Line: 1, Col: 12}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "int8 numbers",
			input: "42i8 123i8 0i8",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "42i8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "123i8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_NUMBER, Str: "0i8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 12}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "hexadecimal numbers with i8 suffix",
			input: "0x42i8 0xdeadbeefi8 0X123ABCi8",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "0x42i8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "0xdeadbeefi8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_NUMBER, Str: "0X123ABCi8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 21}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "mixed number suffixes",
			input: "42 42l 42i8 0x42 0x42l 0x42i8",
			expected: []Lexeme{
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_NUMBER, Str: "42l", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_NUMBER, Str: "42i8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_NUMBER, Str: "0x42", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_NUMBER, Str: "0x42l", Loc: Location{Filename: "test.pirx", Line: 1, Col: 18}},
				{Type: LEX_NUMBER, Str: "0x42i8", Loc: Location{Filename: "test.pirx", Line: 1, Col: 24}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "string literals",
			input: `"hello" "world" "with spaces"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_STRING, Str: "world", Loc: Location{Filename: "test.pirx", Line: 1, Col: 9}},
				{Type: LEX_STRING, Str: "with spaces", Loc: Location{Filename: "test.pirx", Line: 1, Col: 17}},
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
				{Type: LEX_KEYWORD, Str: "func", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "main", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_PUNCTUATION, Str: "(", Loc: Location{Filename: "test.pirx", Line: 1, Col: 10}},
				{Type: LEX_PUNCTUATION, Str: ")", Loc: Location{Filename: "test.pirx", Line: 1, Col: 11}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 2, Col: 5}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 2, Col: 9}},
				{Type: LEX_OPERATOR, Str: "=", Loc: Location{Filename: "test.pirx", Line: 2, Col: 11}},
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 2, Col: 13}},
				{Type: LEX_KEYWORD, Str: "val", Loc: Location{Filename: "test.pirx", Line: 3, Col: 5}},
				{Type: LEX_IDENT, Str: "message", Loc: Location{Filename: "test.pirx", Line: 3, Col: 9}},
				{Type: LEX_OPERATOR, Str: "=", Loc: Location{Filename: "test.pirx", Line: 3, Col: 17}},
				{Type: LEX_STRING, Str: "Hello, Pirx!", Loc: Location{Filename: "test.pirx", Line: 3, Col: 19}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 4, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "multiline input",
			input: "hello\nworld\n42",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 2, Col: 1}},
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 3, Col: 1}},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
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
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 3}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 2, Col: 3}},
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 3, Col: 3}},
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
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
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
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 2, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comment on its own line",
			input: "hello\n// this is a comment\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 3, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "multiple comments",
			input: "hello // comment 1\nworld // comment 2\n42",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 2, Col: 1}},
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 3, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "empty comment",
			input: "hello //\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 2, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comment at end of file",
			input: "hello // comment at EOF",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comment with special characters",
			input: "hello // comment with symbols !@#$%^&*()\nworld",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "world", Loc: Location{Filename: "test.pirx", Line: 2, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "division operator vs comment",
			input: "x / y // division vs comment",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: "/", Loc: Location{Filename: "test.pirx", Line: 1, Col: 3}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 5}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "single slash at EOF",
			input: "hello /",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "hello", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: "/", Loc: Location{Filename: "test.pirx", Line: 1, Col: 7}},
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
				{Type: LEX_KEYWORD, Str: "func", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "main", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_PUNCTUATION, Str: "(", Loc: Location{Filename: "test.pirx", Line: 1, Col: 10}},
				{Type: LEX_PUNCTUATION, Str: ")", Loc: Location{Filename: "test.pirx", Line: 1, Col: 11}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 2, Col: 5}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 2, Col: 9}},
				{Type: LEX_OPERATOR, Str: "=", Loc: Location{Filename: "test.pirx", Line: 2, Col: 11}},
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 2, Col: 13}},
				{Type: LEX_KEYWORD, Str: "return", Loc: Location{Filename: "test.pirx", Line: 4, Col: 5}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 4, Col: 12}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 5, Col: 1}},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
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
				{Type: LEX_STRING, Str: "hello\nworld", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "tab escape",
			input: `"hello\tworld"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\tworld", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "carriage return escape",
			input: `"hello\rworld"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\rworld", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "backslash escape",
			input: `"hello\\world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\\world", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "quote escape",
			input: `"hello\"world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\"world", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "single quote escape",
			input: `"hello\'world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello'world", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "multiple escapes",
			input: `"line1\nline2\tcolumn2\nline3"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "line1\nline2\tcolumn2\nline3", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "escape at end of string",
			input: `"hello world\n"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello world\n", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "escape at start of string",
			input: `"\nhello world"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "\nhello world", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "consecutive escapes",
			input: `"\\n\\t\\r"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "\\n\\t\\r", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "empty string with escape",
			input: `"\n"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "\n", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
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
				{Type: LEX_KEYWORD, Str: "func", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "main", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_PUNCTUATION, Str: "(", Loc: Location{Filename: "test.pirx", Line: 1, Col: 10}},
				{Type: LEX_PUNCTUATION, Str: ")", Loc: Location{Filename: "test.pirx", Line: 1, Col: 11}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 2, Col: 5}},
				{Type: LEX_IDENT, Str: "message", Loc: Location{Filename: "test.pirx", Line: 2, Col: 9}},
				{Type: LEX_OPERATOR, Str: "=", Loc: Location{Filename: "test.pirx", Line: 2, Col: 17}},
				{Type: LEX_STRING, Str: "Hello\nWorld\tPirx!", Loc: Location{Filename: "test.pirx", Line: 2, Col: 19}},
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 3, Col: 5}},
				{Type: LEX_IDENT, Str: "path", Loc: Location{Filename: "test.pirx", Line: 3, Col: 9}},
				{Type: LEX_OPERATOR, Str: "=", Loc: Location{Filename: "test.pirx", Line: 3, Col: 14}},
				{Type: LEX_STRING, Str: "C:\\Users\\test\\file.txt", Loc: Location{Filename: "test.pirx", Line: 3, Col: 16}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 4, Col: 1}},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
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
				{Type: LEX_STRING, Str: "helloxworld", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}}, // \x becomes just x
				{Type: LEX_EOF},
			},
		},
		{
			name:  "backslash at end of string",
			input: `"hello\\"`,
			expected: []Lexeme{
				{Type: LEX_STRING, Str: "hello\\", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
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
			l := New(strings.NewReader(tt.input), "test.pirx")
			_, err := l.Next()
			if (err != nil) != tt.expectError {
				t.Errorf("expected error: %v, got: %v", tt.expectError, err)
			}
		})
	}
}

func TestLexerPointerTypes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Lexeme
	}{
		{
			name:  "simple pointer type",
			input: "*int",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 2}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "pointer to pointer type",
			input: "**int",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 2}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 3}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "pointer type in variable declaration",
			input: "var ptr: *int",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "ptr", Loc: Location{Filename: "test.pirx", Line: 1, Col: 5}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 10}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 11}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "pointer type in function signature",
			input: "func test(x: *int, y: **string): *bool",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "func", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "test", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_PUNCTUATION, Str: "(", Loc: Location{Filename: "test.pirx", Line: 1, Col: 10}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 11}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 12}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 14}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 15}},
				{Type: LEX_PUNCTUATION, Str: ",", Loc: Location{Filename: "test.pirx", Line: 1, Col: 18}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 20}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 21}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 23}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 24}},
				{Type: LEX_IDENT, Str: "string", Loc: Location{Filename: "test.pirx", Line: 1, Col: 25}},
				{Type: LEX_PUNCTUATION, Str: ")", Loc: Location{Filename: "test.pirx", Line: 1, Col: 31}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 32}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 34}},
				{Type: LEX_IDENT, Str: "bool", Loc: Location{Filename: "test.pirx", Line: 1, Col: 35}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "mixed pointer types and multiplication",
			input: "var x: *int; y = 2 * 3",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "var", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 5}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 9}},
				{Type: LEX_PUNCTUATION, Str: ";", Loc: Location{Filename: "test.pirx", Line: 1, Col: 12}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 14}},
				{Type: LEX_OPERATOR, Str: "=", Loc: Location{Filename: "test.pirx", Line: 1, Col: 16}},
				{Type: LEX_NUMBER, Str: "2", Loc: Location{Filename: "test.pirx", Line: 1, Col: 18}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 20}},
				{Type: LEX_NUMBER, Str: "3", Loc: Location{Filename: "test.pirx", Line: 1, Col: 22}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "pointer type with struct",
			input: "struct Node { data: int; next: *Node; }",
			expected: []Lexeme{
				{Type: LEX_KEYWORD, Str: "struct", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "Node", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_IDENT, Str: "data", Loc: Location{Filename: "test.pirx", Line: 1, Col: 15}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 19}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 21}},
				{Type: LEX_PUNCTUATION, Str: ";", Loc: Location{Filename: "test.pirx", Line: 1, Col: 24}},
				{Type: LEX_IDENT, Str: "next", Loc: Location{Filename: "test.pirx", Line: 1, Col: 26}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 30}},
				{Type: LEX_OPERATOR, Str: "*", Loc: Location{Filename: "test.pirx", Line: 1, Col: 32}},
				{Type: LEX_IDENT, Str: "Node", Loc: Location{Filename: "test.pirx", Line: 1, Col: 33}},
				{Type: LEX_PUNCTUATION, Str: ";", Loc: Location{Filename: "test.pirx", Line: 1, Col: 37}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 1, Col: 39}},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
				}
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
				{Type: LEX_OPERATOR, Str: "==", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: "!=", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "comparison operators",
			input: "< > <= >=",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "<", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: ">", Loc: Location{Filename: "test.pirx", Line: 1, Col: 3}},
				{Type: LEX_OPERATOR, Str: "<=", Loc: Location{Filename: "test.pirx", Line: 1, Col: 5}},
				{Type: LEX_OPERATOR, Str: ">=", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "logical operators",
			input: "&& || !",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "&&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: "||", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_OPERATOR, Str: "!", Loc: Location{Filename: "test.pirx", Line: 1, Col: 7}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "address-of operator",
			input: "&x &y",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 2}},
				{Type: LEX_OPERATOR, Str: "&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 5}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "mixed address-of and logical operators",
			input: "&x && &y",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 2}},
				{Type: LEX_OPERATOR, Str: "&&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 4}},
				{Type: LEX_OPERATOR, Str: "&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 7}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "mixed boolean operations",
			input: "x == 42 && y != 0",
			expected: []Lexeme{
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_OPERATOR, Str: "==", Loc: Location{Filename: "test.pirx", Line: 1, Col: 3}},
				{Type: LEX_NUMBER, Str: "42", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_OPERATOR, Str: "&&", Loc: Location{Filename: "test.pirx", Line: 1, Col: 9}},
				{Type: LEX_IDENT, Str: "y", Loc: Location{Filename: "test.pirx", Line: 1, Col: 12}},
				{Type: LEX_OPERATOR, Str: "!=", Loc: Location{Filename: "test.pirx", Line: 1, Col: 14}},
				{Type: LEX_NUMBER, Str: "0", Loc: Location{Filename: "test.pirx", Line: 1, Col: 17}},
				{Type: LEX_EOF},
			},
		},
		{
			name:  "negation and comparisons",
			input: "!found || x >= limit",
			expected: []Lexeme{
				{Type: LEX_OPERATOR, Str: "!", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "found", Loc: Location{Filename: "test.pirx", Line: 1, Col: 2}},
				{Type: LEX_OPERATOR, Str: "||", Loc: Location{Filename: "test.pirx", Line: 1, Col: 8}},
				{Type: LEX_IDENT, Str: "x", Loc: Location{Filename: "test.pirx", Line: 1, Col: 11}},
				{Type: LEX_OPERATOR, Str: ">=", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_IDENT, Str: "limit", Loc: Location{Filename: "test.pirx", Line: 1, Col: 16}},
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
				{Type: LEX_KEYWORD, Str: "func", Loc: Location{Filename: "test.pirx", Line: 1, Col: 1}},
				{Type: LEX_IDENT, Str: "compare", Loc: Location{Filename: "test.pirx", Line: 1, Col: 6}},
				{Type: LEX_PUNCTUATION, Str: "(", Loc: Location{Filename: "test.pirx", Line: 1, Col: 13}},
				{Type: LEX_IDENT, Str: "a", Loc: Location{Filename: "test.pirx", Line: 1, Col: 14}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 15}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 17}},
				{Type: LEX_PUNCTUATION, Str: ",", Loc: Location{Filename: "test.pirx", Line: 1, Col: 20}},
				{Type: LEX_IDENT, Str: "b", Loc: Location{Filename: "test.pirx", Line: 1, Col: 22}},
				{Type: LEX_PUNCTUATION, Str: ":", Loc: Location{Filename: "test.pirx", Line: 1, Col: 23}},
				{Type: LEX_IDENT, Str: "int", Loc: Location{Filename: "test.pirx", Line: 1, Col: 25}},
				{Type: LEX_PUNCTUATION, Str: ")", Loc: Location{Filename: "test.pirx", Line: 1, Col: 28}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 1, Col: 30}},
				{Type: LEX_KEYWORD, Str: "if", Loc: Location{Filename: "test.pirx", Line: 2, Col: 5}},
				{Type: LEX_IDENT, Str: "a", Loc: Location{Filename: "test.pirx", Line: 2, Col: 8}},
				{Type: LEX_OPERATOR, Str: "<=", Loc: Location{Filename: "test.pirx", Line: 2, Col: 10}},
				{Type: LEX_IDENT, Str: "b", Loc: Location{Filename: "test.pirx", Line: 2, Col: 13}},
				{Type: LEX_OPERATOR, Str: "&&", Loc: Location{Filename: "test.pirx", Line: 2, Col: 15}},
				{Type: LEX_IDENT, Str: "a", Loc: Location{Filename: "test.pirx", Line: 2, Col: 18}},
				{Type: LEX_OPERATOR, Str: "!=", Loc: Location{Filename: "test.pirx", Line: 2, Col: 20}},
				{Type: LEX_NUMBER, Str: "0", Loc: Location{Filename: "test.pirx", Line: 2, Col: 23}},
				{Type: LEX_PUNCTUATION, Str: "{", Loc: Location{Filename: "test.pirx", Line: 2, Col: 25}},
				{Type: LEX_KEYWORD, Str: "return", Loc: Location{Filename: "test.pirx", Line: 3, Col: 9}},
				{Type: LEX_IDENT, Str: "a", Loc: Location{Filename: "test.pirx", Line: 3, Col: 16}},
				{Type: LEX_OPERATOR, Str: "<", Loc: Location{Filename: "test.pirx", Line: 3, Col: 18}},
				{Type: LEX_IDENT, Str: "b", Loc: Location{Filename: "test.pirx", Line: 3, Col: 20}},
				{Type: LEX_OPERATOR, Str: "||", Loc: Location{Filename: "test.pirx", Line: 3, Col: 22}},
				{Type: LEX_IDENT, Str: "a", Loc: Location{Filename: "test.pirx", Line: 3, Col: 25}},
				{Type: LEX_OPERATOR, Str: "==", Loc: Location{Filename: "test.pirx", Line: 3, Col: 27}},
				{Type: LEX_IDENT, Str: "b", Loc: Location{Filename: "test.pirx", Line: 3, Col: 30}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 4, Col: 5}},
				{Type: LEX_PUNCTUATION, Str: "}", Loc: Location{Filename: "test.pirx", Line: 5, Col: 1}},
				{Type: LEX_EOF},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := New(strings.NewReader(tt.input), "test.pirx")
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
				if got.Loc.Line != expected.Loc.Line {
					t.Errorf("token %d: expected line %d, got %d", i, expected.Loc.Line, got.Loc.Line)
				}
				if got.Loc.Col != expected.Loc.Col {
					t.Errorf("token %d: expected column %d, got %d", i, expected.Loc.Col, got.Loc.Col)
				}
			}
		})
	}
}
