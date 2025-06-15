package lexer

import (
	"bufio"
	"io"
	"unicode"
)

// Token types
const (
	LEX_EOF = iota
	LEX_IDENT
	LEX_NUMBER
	LEX_STRING
	LEX_KEYWORD
	LEX_OPERATOR
	LEX_PUNCTUATION
)

// Keywords in Pirx
var keywords = map[string]bool{
	"func":   true,
	"var":    true,
	"val":    true,
	"if":     true,
	"else":   true,
	"for":    true,
	"while":  true,
	"return": true,
	"true":   true,
	"false":  true,
}

// Single-character operators and punctuation
var singleCharTokens = map[rune]int{
	'(': LEX_PUNCTUATION,
	')': LEX_PUNCTUATION,
	'{': LEX_PUNCTUATION,
	'}': LEX_PUNCTUATION,
	'[': LEX_PUNCTUATION,
	']': LEX_PUNCTUATION,
	';': LEX_PUNCTUATION,
	',': LEX_PUNCTUATION,
	'=': LEX_OPERATOR,
	'+': LEX_OPERATOR,
	'-': LEX_OPERATOR,
	'*': LEX_OPERATOR,
	'/': LEX_OPERATOR,
	'%': LEX_OPERATOR,
	'<': LEX_OPERATOR,
	'>': LEX_OPERATOR,
	'!': LEX_OPERATOR,
}

type Lexeme struct {
	Type int
	Str  string
	Line int
	Col  int
}

type Lexer struct {
	input     *bufio.Reader
	line      int
	col       int
	prevCol   int
	lastRune  rune
	lastSize  int
	hasUnread bool
}

func New(inputReader io.Reader) *Lexer {
	return &Lexer{
		input:   bufio.NewReader(inputReader),
		line:    1,
		col:     1,
		prevCol: 1,
	}
}

// readRune reads the next rune from the input
func (l *Lexer) readRune() (rune, int, error) {
	var r rune
	var size int
	var err error

	if l.hasUnread {
		l.hasUnread = false
		r, size, err = l.lastRune, l.lastSize, nil
	} else {
		l.prevCol = l.col
		r, size, err = l.input.ReadRune()
	}

	if err != nil {
		return 0, 0, err
	}

	l.lastRune = r
	l.lastSize = size
	if r == '\n' {
		l.line++
		l.col = 1
	} else {
		l.col++
	}
	return r, size, nil
}

// unreadRune puts back the last read rune.
// Should be called at most once per readRune.
func (l *Lexer) unreadRune() {
	l.hasUnread = true
	if l.lastRune == '\n' {
		l.line--
	}
	l.col = l.prevCol
}

// skipSpace skips whitespace characters
func (l *Lexer) skipSpace() error {
	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				return nil
			}
			return err
		}
		if !unicode.IsSpace(r) {
			l.unreadRune()
			return nil
		}
	}
}

// Next returns the next lexeme from the input
func (l *Lexer) Next() (Lexeme, error) {
	// Skip whitespace
	if err := l.skipSpace(); err != nil {
		return Lexeme{Type: LEX_EOF}, err
	}
	// Start position of the lexeme
	startLine := l.line
	startCol := l.col
	// Read the first character
	r, _, err := l.readRune()
	if err != nil {
		if err == io.EOF {
			return Lexeme{Type: LEX_EOF}, nil
		}
		return Lexeme{Type: LEX_EOF}, err
	}
	// Handle different token types
	switch {
	case unicode.IsLetter(r) || r == '_':
		// Identifier or keyword
		l.unreadRune()
		return l.lexIdent(startLine, startCol)
	case r == '"':
		// String literal
		return l.lexString(startLine, startCol)
	case unicode.IsDigit(r):
		// Number
		l.unreadRune()
		return l.lexNumber(startLine, startCol)
	default:
		// Check for single-character tokens
		if tokenType, ok := singleCharTokens[r]; ok {
			return Lexeme{
				Type: tokenType,
				Str:  string(r),
				Line: startLine,
				Col:  startCol,
			}, nil
		}
		// For now, return EOF for unknown characters
		return Lexeme{Type: LEX_EOF}, nil
	}
}

// lexIdent reads an identifier or keyword
func (l *Lexer) lexIdent(startLine, startCol int) (Lexeme, error) {
	var ident string

	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return Lexeme{}, err
		}

		if !unicode.IsLetter(r) && !unicode.IsDigit(r) && r != '_' {
			l.unreadRune()
			break
		}

		ident += string(r)
	}

	// Check if it's a keyword
	if keywords[ident] {
		return Lexeme{
			Type: LEX_KEYWORD,
			Str:  ident,
			Line: startLine,
			Col:  startCol,
		}, nil
	}

	return Lexeme{
		Type: LEX_IDENT,
		Str:  ident,
		Line: startLine,
		Col:  startCol,
	}, nil
}

// lexString reads a string literal
func (l *Lexer) lexString(startLine, startCol int) (Lexeme, error) {
	var str string

	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				return Lexeme{}, io.ErrUnexpectedEOF
			}
			return Lexeme{}, err
		}

		if r == '"' {
			return Lexeme{
				Type: LEX_STRING,
				Str:  str,
				Line: startLine,
				Col:  startCol,
			}, nil
		}

		if r == '\\' {
			// Handle escape sequences (to be implemented)
			continue
		}

		str += string(r)
	}
}

// lexNumber reads a number literal
func (l *Lexer) lexNumber(startLine, startCol int) (Lexeme, error) {
	var num string

	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return Lexeme{}, err
		}

		if !unicode.IsDigit(r) {
			l.unreadRune()
			break
		}

		num += string(r)
	}

	return Lexeme{
		Type: LEX_NUMBER,
		Str:  num,
		Line: startLine,
		Col:  startCol,
	}, nil
}
