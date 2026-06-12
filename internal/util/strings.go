package util

import (
	"fmt"
	"strings"
)

// EscapeString encodes s for use inside a double-quoted assembler string
// directive. It iterates over bytes, not runes, so the assembled data is
// byte-for-byte identical to s: the compiler computes string lengths from
// the Go byte length, and any re-encoding here would make the two disagree.
// Non-printable bytes are emitted as fixed-width 3-digit octal escapes
// (\NNN), which GNU as and Darwin as parse identically and which, unlike
// \x escapes, cannot merge with hex-digit characters that follow them.
func EscapeString(s string) string {
	sb := strings.Builder{}
	for i := 0; i < len(s); i++ {
		b := s[i]
		switch {
		case b == '"':
			sb.WriteString("\\\"")
		case b == '\\':
			sb.WriteString("\\\\")
		case b == '\n':
			sb.WriteString("\\n")
		case b == '\t':
			sb.WriteString("\\t")
		case b >= 0x20 && b < 0x7F:
			sb.WriteByte(b)
		default:
			sb.WriteString(fmt.Sprintf("\\%03o", b))
		}
	}
	return sb.String()
}
