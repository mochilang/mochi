//go:build slow

package any2mochi

import (
	"testing"

	pycode "mochi/compile/py"
)

func TestParsePython(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, "pyright-langserver")
	src := "def add(x,y):\n    return x + y"
	syms, diags, err := ParseText("pyright-langserver", []string{"--stdio"}, "python", src)
	if err != nil {
		t.Fatalf("parse python: %v", err)
	}
	if len(diags) > 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}
