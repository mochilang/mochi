package vm_test

import (
	"bytes"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVM_ErrorStackTrace(t *testing.T) {
	src := `
fun g() {
    let x = 1 / 0
}
fun f() {
    g()
}
f()
`
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err == nil {
		t.Fatal("expected error")
	}
	result := out.String()
	if !strings.Contains(result, "stack trace:") || !strings.Contains(result, "call graph:") {
		t.Fatalf("expected stack trace output, got: %s", result)
	}
}
