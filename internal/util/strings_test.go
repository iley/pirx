package util

import "testing"

func TestEscapeString(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "empty string",
			input:    "",
			expected: "",
		},
		{
			name:     "simple string",
			input:    "hello",
			expected: "hello",
		},
		{
			name:     "string with double quotes",
			input:    `hello "world"`,
			expected: `hello \"world\"`,
		},
		{
			name:     "string with newline",
			input:    "hello\nworld",
			expected: "hello\\nworld",
		},
		{
			name:     "string with tab",
			input:    "hello\tworld",
			expected: "hello\\tworld",
		},
		{
			name:     "string with all escape sequences",
			input:    "hello \"world\"\n\ttab",
			expected: "hello \\\"world\\\"\\n\\ttab",
		},
		{
			name:     "string with non-printable ASCII",
			input:    "hello\x00world",
			expected: "hello\\x00world",
		},
		{
			name:     "string with high ASCII",
			input:    "hello\x80world",
			expected: "hello\\xFFFDworld",
		},
		{
			name:     "string with control characters",
			input:    "hello\x01\x02\x03world",
			expected: "hello\\x01\\x02\\x03world",
		},
		{
			name:     "string with backslash",
			input:    "hello\\world",
			expected: "hello\\world",
		},
		{
			name:     "string with carriage return",
			input:    "hello\rworld",
			expected: "hello\\x0Dworld",
		},
		{
			name:     "string with form feed",
			input:    "hello\fworld",
			expected: "hello\\x0Cworld",
		},
		{
			name:     "string with vertical tab",
			input:    "hello\vworld",
			expected: "hello\\x0Bworld",
		},
		{
			name:     "string with bell character",
			input:    "hello\aworld",
			expected: "hello\\x07world",
		},
		{
			name:     "string with backspace",
			input:    "hello\bworld",
			expected: "hello\\x08world",
		},
		{
			name:     "printable ASCII characters",
			input:    "!@#$%^&*()_+=-[]{}|;':,.<>?",
			expected: "!@#$%^&*()_+=-[]{}|;':,.<>?",
		},
		{
			name:     "numbers and letters",
			input:    "abc123XYZ",
			expected: "abc123XYZ",
		},
		{
			name:     "only double quotes",
			input:    `"""`,
			expected: `\"\"\"`,
		},
		{
			name:     "only newlines",
			input:    "\n\n\n",
			expected: "\\n\\n\\n",
		},
		{
			name:     "only tabs",
			input:    "\t\t\t",
			expected: "\\t\\t\\t",
		},
		{
			name:     "mixed whitespace",
			input:    " \t\n ",
			expected: " \\t\\n ",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := EscapeString(tt.input)
			if got != tt.expected {
				t.Errorf("EscapeString(%q) = %q, want %q", tt.input, got, tt.expected)
			}
		})
	}
}
