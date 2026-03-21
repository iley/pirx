package lexer

import (
	"strings"
	"testing"
)

func tokenize(input string) ([]Lexeme, error) {
	l := New(strings.NewReader(input), "test")
	var tokens []Lexeme
	for {
		tok, err := l.Next()
		if err != nil {
			return nil, err
		}
		if tok.Type == LEX_EOF {
			break
		}
		tokens = append(tokens, tok)
	}
	return tokens, nil
}

func TestNumberFollowedByIdentStartingWithI(t *testing.T) {
	// When a number is followed by 'i' + non-'8' char, the lexer calls
	// unreadRune() twice. The second call doesn't actually restore the 'i',
	// so the 'i' gets lost.
	tokens, err := tokenize("123 identifier")
	if err != nil {
		t.Fatal(err)
	}
	if len(tokens) != 2 {
		t.Fatalf("expected 2 tokens, got %d: %v", len(tokens), tokens)
	}

	// Now the real test: number immediately followed by identifier starting with 'i'
	// (no space). The lexer should produce NUMBER("123") IDENT("id").
	tokens, err = tokenize("123id")
	if err != nil {
		t.Fatal(err)
	}
	if len(tokens) != 2 {
		t.Fatalf("expected 2 tokens, got %d: %v", len(tokens), tokens)
	}
	if tokens[0].Type != LEX_NUMBER || tokens[0].Str != "123" {
		t.Errorf("token 0: expected NUMBER '123', got %s '%s'", tokens[0].Type, tokens[0].Str)
	}
	if tokens[1].Type != LEX_IDENT || tokens[1].Str != "id" {
		t.Errorf("token 1: expected IDENT 'id', got %s '%s'", tokens[1].Type, tokens[1].Str)
	}
}

func TestHexNumberFollowedByIdentStartingWithI(t *testing.T) {
	// Same bug in lexHexNumber
	tokens, err := tokenize("0xFFid")
	if err != nil {
		t.Fatal(err)
	}
	if len(tokens) != 2 {
		t.Fatalf("expected 2 tokens, got %d: %v", len(tokens), tokens)
	}
	if tokens[0].Type != LEX_NUMBER || tokens[0].Str != "0xFF" {
		t.Errorf("token 0: expected NUMBER '0xFF', got %s '%s'", tokens[0].Type, tokens[0].Str)
	}
	if tokens[1].Type != LEX_IDENT || tokens[1].Str != "id" {
		t.Errorf("token 1: expected IDENT 'id', got %s '%s'", tokens[1].Type, tokens[1].Str)
	}
}
