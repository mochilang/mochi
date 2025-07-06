//go:build slow

package golang

import (
	"os/exec"
	"testing"

	gocode "mochi/compile/go"
)

func requireBinary(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath(name); err != nil {
		t.Skipf("%s not found", name)
	}
}

func TestParseGo(t *testing.T) {
	_ = gocode.EnsureGopls()
	requireBinary(t, "gopls")
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	syms, diags, err := ParseText("gopls", nil, "go", src)
	if err != nil {
		t.Fatalf("parse go: %v", err)
	}
	if len(diags) > 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}
