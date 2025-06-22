package util

import (
	"fmt"
	"strings"
	"unicode"
)

func EscapeString(s string) string {
	sb := strings.Builder{}
	for _, rune := range s {
		if unicode.IsPrint(rune) && rune < unicode.MaxASCII {
			sb.WriteRune(rune)
		} else {
			sb.WriteString(fmt.Sprintf("\\x%02X", rune))
		}
	}
	return sb.String()
}
