package stackvm

import (
	"strings"
	"testing"

	"mochi/parser"
)

func TestDisassemble(t *testing.T) {
	src := `fun add(a:int,b:int): int { return a + b }
print(add(1,2))`
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	p, err := Compile(prog)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	out := p.Disassemble(src)
	if !strings.Contains(out, "func main") || !strings.Contains(out, "func add") {
		t.Fatalf("missing function names in disassembly:\n%s", out)
	}
	if !strings.Contains(out, "Add") {
		t.Fatalf("expected Add opcode in disassembly:\n%s", out)
	}
}
