package ast

import "testing"

// Pointer-identity comparisons like typ == ast.File are used throughout the
// compiler, so NewBaseType must return the singleton for every known name.
func TestNewBaseTypeInternsAllSingletons(t *testing.T) {
	for _, singleton := range baseTypeSingletons {
		if got := NewBaseType(singleton.Name); got != singleton {
			t.Errorf("NewBaseType(%q) returned a new instance instead of the singleton", singleton.Name)
		}
	}
}

func TestNewBaseTypeUnknownName(t *testing.T) {
	typ := NewBaseType("mystruct")
	if typ.Name != "mystruct" {
		t.Errorf("NewBaseType(\"mystruct\").Name = %q", typ.Name)
	}
}
