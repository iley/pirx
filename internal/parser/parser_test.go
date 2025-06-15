package parser

import (
	"strings"
	"testing"

	"github.com/iley/pirx/internal/lexer"
)

func TestParseTrivialProgram(t *testing.T) {
	src := `func main() {}`

	lex := lexer.New(strings.NewReader(src))
	parser := New(lex)
	prog, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("ParseProgram() error = %v", err)
	}
	if prog == nil {
		t.Fatal("ParseProgram() returned nil program")
	}
	if len(prog.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(prog.Functions))
	}
	fn := prog.Functions[0]
	if fn.Name != "main" {
		t.Errorf("expected function name 'main', got %q", fn.Name)
	}
	if len(fn.Params) != 0 {
		t.Errorf("expected 0 params, got %d", len(fn.Params))
	}
	if fn.Body == nil || len(fn.Body.Statements) != 0 {
		t.Errorf("expected empty function body, got %+v", fn.Body)
	}
}

func TestParseFunctionWithVarDeclaration(t *testing.T) {
	src := `func main() { var x int }`

	lex := lexer.New(strings.NewReader(src))
	parser := New(lex)
	prog, err := parser.ParseProgram()
	if err != nil {
		t.Fatalf("ParseProgram() error = %v", err)
	}
	if prog == nil {
		t.Fatal("ParseProgram() returned nil program")
	}
	if len(prog.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(prog.Functions))
	}
	fn := prog.Functions[0]
	if fn.Name != "main" {
		t.Errorf("expected function name 'main', got %q", fn.Name)
	}
	if fn.Body == nil {
		t.Fatalf("expected function body, got nil")
	}
	if len(fn.Body.Statements) != 1 {
		t.Fatalf("expected 1 statement, got %d", len(fn.Body.Statements))
	}
	stmt := fn.Body.Statements[0]
	varDecl, ok := stmt.(*VariableDeclaration)
	if !ok {
		t.Fatalf("expected statement to be *VariableDeclaration, got %T", stmt)
	}
	if varDecl.Name != "x" {
		t.Errorf("expected var name 'x', got %q", varDecl.Name)
	}
	if varDecl.Type != "int" {
		t.Errorf("expected var type 'int', got %q", varDecl.Type)
	}
}
