package ir

import (
	"strings"
)

func IsGlobal(name string) bool {
	return strings.HasPrefix(name, "@")
}

func IsIntermediary(name string) bool {
	return strings.HasPrefix(name, "$")
}
