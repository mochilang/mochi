//go:build archived && slow

package py

import (
	"os/exec"
	"testing"

	pycode "mochi/archived/py"
)

func requireBinary(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath(name); err != nil {
		t.Skipf("%s not found", name)
	}
}

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
