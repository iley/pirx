package util

import (
	"fmt"
	"strings"
	"unicode"
)

func EscapeString(s string) string {
	sb := strings.Builder{}
	for _, rune := range s {
		if rune == '"' {
			sb.WriteString("\\\"")
		} else if rune == '\n' {
			sb.WriteString("\\n")
		} else if rune == '\t' {
			sb.WriteString("\\t")
		} else if unicode.IsPrint(rune) && rune < unicode.MaxASCII {
			sb.WriteRune(rune)
		} else {
			sb.WriteString(fmt.Sprintf("\\x%02X", rune))
		}
	}
	return sb.String()
}
