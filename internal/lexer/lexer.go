package lexer

import (
	"bufio"
	"fmt"
	"io"
	"unicode"
)

type TokenType int

// Token types
const (
	LEX_EOF TokenType = iota
	LEX_IDENT
	LEX_NUMBER
	LEX_STRING
	LEX_KEYWORD
	LEX_OPERATOR
	LEX_PUNCTUATION
)

func (t TokenType) String() string {
	switch t {
	case LEX_EOF:
		return "EOF"
	case LEX_IDENT:
		return "IDENT"
	case LEX_NUMBER:
		return "NUMBER"
	case LEX_STRING:
		return "STRING"
	case LEX_KEYWORD:
		return "KEYWORD"
	case LEX_OPERATOR:
		return "OPERATOR"
	case LEX_PUNCTUATION:
		return "PUNCTUATION"
	default:
		return "UNKNOWN"
	}
}

// Keywords in Pirx
var keywords = map[string]bool{
	"func":     true,
	"var":      true,
	"val":      true,
	"if":       true,
	"else":     true,
	"for":      true,
	"while":    true,
	"return":   true,
	"break":    true,
	"continue": true,
	"true":     true,
	"false":    true,
	"extern":   true,
	"struct":   true,
}

// Single-character operators and punctuation
var singleCharTokens = map[rune]TokenType{
	'(': LEX_PUNCTUATION,
	')': LEX_PUNCTUATION,
	'{': LEX_PUNCTUATION,
	'}': LEX_PUNCTUATION,
	'[': LEX_PUNCTUATION,
	']': LEX_PUNCTUATION,
	';': LEX_PUNCTUATION,
	',': LEX_PUNCTUATION,
	':': LEX_PUNCTUATION,
	'+': LEX_OPERATOR,
	'-': LEX_OPERATOR,
	'*': LEX_OPERATOR,
	'%': LEX_OPERATOR,
}

type Lexeme struct {
	Type TokenType
	Str  string
	Line int
	Col  int
}

func (l Lexeme) String() string {
	if l.Str == "" {
		return fmt.Sprintf("<%s>", l.Type)
	}
	return fmt.Sprintf("<%s %q>", l.Type, l.Str)
}

func (l Lexeme) IsKeyword(kv string) bool {
	return l.Type == LEX_KEYWORD && l.Str == kv
}

func (l Lexeme) IsPunctuation(pv string) bool {
	return l.Type == LEX_PUNCTUATION && l.Str == pv
}

func (l Lexeme) IsOperator(op string) bool {
	return l.Type == LEX_OPERATOR && l.Str == op
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

// skipComment skips a C++ style comment (from // to end of line)
func (l *Lexer) skipComment() error {
	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				return nil
			}
			return err
		}
		if r == '\n' {
			// Don't unread the newline - we want to consume it
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
	case r == '/':
		// Check for comment
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Just a single '/' at EOF
				return Lexeme{
					Type: LEX_OPERATOR,
					Str:  "/",
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '/' {
			// It's a comment, skip to end of line
			if err := l.skipComment(); err != nil {
				return Lexeme{Type: LEX_EOF}, err
			}
			// Recursively call Next to get the next token after the comment
			return l.Next()
		} else {
			// It's just a division operator, put back the second character
			l.unreadRune()
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "/",
				Line: startLine,
				Col:  startCol,
			}, nil
		}
	case r == '=':
		// Check for equality operator
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Just a single '=' at EOF
				return Lexeme{
					Type: LEX_OPERATOR,
					Str:  "=",
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '=' {
			// It's an equality operator
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "==",
				Line: startLine,
				Col:  startCol,
			}, nil
		} else {
			// It's just an assignment operator, put back the second character
			l.unreadRune()
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "=",
				Line: startLine,
				Col:  startCol,
			}, nil
		}
	case r == '!':
		// Check for not-equal operator
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Just a single '!' at EOF
				return Lexeme{
					Type: LEX_OPERATOR,
					Str:  "!",
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '=' {
			// It's a not-equal operator
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "!=",
				Line: startLine,
				Col:  startCol,
			}, nil
		} else {
			// It's just a negation operator, put back the second character
			l.unreadRune()
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "!",
				Line: startLine,
				Col:  startCol,
			}, nil
		}
	case r == '<':
		// Check for less-than-or-equal operator
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Just a single '<' at EOF
				return Lexeme{
					Type: LEX_OPERATOR,
					Str:  "<",
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '=' {
			// It's a less-than-or-equal operator
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "<=",
				Line: startLine,
				Col:  startCol,
			}, nil
		} else {
			// It's just a less-than operator, put back the second character
			l.unreadRune()
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "<",
				Line: startLine,
				Col:  startCol,
			}, nil
		}
	case r == '>':
		// Check for greater-than-or-equal operator
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Just a single '>' at EOF
				return Lexeme{
					Type: LEX_OPERATOR,
					Str:  ">",
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '=' {
			// It's a greater-than-or-equal operator
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  ">=",
				Line: startLine,
				Col:  startCol,
			}, nil
		} else {
			// It's just a greater-than operator, put back the second character
			l.unreadRune()
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  ">",
				Line: startLine,
				Col:  startCol,
			}, nil
		}
	case r == '&':
		// Check for logical AND operator or address-of operator
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Single '&' at EOF - address-of operator
				return Lexeme{
					Type: LEX_OPERATOR,
					Str:  "&",
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '&' {
			// It's a logical AND operator
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "&&",
				Line: startLine,
				Col:  startCol,
			}, nil
		} else {
			// Single '&' is address-of operator
			l.unreadRune()
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "&",
				Line: startLine,
				Col:  startCol,
			}, nil
		}
	case r == '|':
		// Check for logical OR operator
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Single '|' at EOF - not a valid operator, return EOF
				return Lexeme{Type: LEX_EOF}, nil
			}
			return Lexeme{Type: LEX_EOF}, err
		}
		if nextR == '|' {
			// It's a logical OR operator
			return Lexeme{
				Type: LEX_OPERATOR,
				Str:  "||",
				Line: startLine,
				Col:  startCol,
			}, nil
		} else {
			// Single '|' is not supported, return EOF for unknown character
			l.unreadRune()
			return Lexeme{Type: LEX_EOF}, nil
		}
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
			// Handle escape sequences
			nextR, _, err := l.readRune()
			if err != nil {
				if err == io.EOF {
					// Backslash at end of file - treat as literal backslash
					str += string(r)
					continue
				}
				return Lexeme{}, err
			}

			// Process escape sequence
			switch nextR {
			case 'n':
				str += "\n"
			case 't':
				str += "\t"
			case 'r':
				str += "\r"
			case '\\':
				str += "\\"
			case '"':
				str += "\""
			case '\'':
				str += "'"
			default:
				// Unknown escape sequence - treat as literal character (remove backslash)
				str += string(nextR)
			}
			continue
		}

		str += string(r)
	}
}

// lexNumber reads a number literal
func (l *Lexer) lexNumber(startLine, startCol int) (Lexeme, error) {
	var num string

	// Read the first digit
	r, _, err := l.readRune()
	if err != nil {
		return Lexeme{}, err
	}
	num += string(r)

	// Check for hexadecimal prefix
	if r == '0' {
		nextR, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				// Just "0" at EOF
				return Lexeme{
					Type: LEX_NUMBER,
					Str:  num,
					Line: startLine,
					Col:  startCol,
				}, nil
			}
			return Lexeme{}, err
		}
		if nextR == 'x' || nextR == 'X' {
			// It's a hexadecimal number
			num += string(nextR)
			return l.lexHexNumber(num, startLine, startCol)
		} else {
			// Not hex, put back the character and continue with decimal
			l.unreadRune()
		}
	}

	// Continue reading decimal digits
	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return Lexeme{}, err
		}

		// "l" suffix indicates an int64 literal
		if r == 'l' {
			num += string(r)
			break
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

// lexHexNumber reads a hexadecimal number literal
func (l *Lexer) lexHexNumber(prefix string, startLine, startCol int) (Lexeme, error) {
	var num string = prefix

	for {
		r, _, err := l.readRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return Lexeme{}, err
		}

		// "l" suffix indicates an int64 literal
		if r == 'l' {
			num += string(r)
			break
		}

		// Check for valid hex digit
		if !((r >= '0' && r <= '9') || (r >= 'a' && r <= 'f') || (r >= 'A' && r <= 'F')) {
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
