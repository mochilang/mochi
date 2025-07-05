package any2mochi

import (
	"os/exec"
	"testing"

	gocode "mochi/compile/go"
	pycode "mochi/compile/py"
	tscode "mochi/compile/ts"
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
	syms, err := ParseText("gopls", nil, "go", src)
	if err != nil {
		t.Fatalf("parse go: %v", err)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}

func TestParsePython(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, "pyright-langserver")
	src := "def add(x,y):\n    return x + y"
	syms, err := ParseText("pyright-langserver", []string{"--stdio"}, "python", src)
	if err != nil {
		t.Fatalf("parse python: %v", err)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}

func TestParseTypeScript(t *testing.T) {
	_ = tscode.EnsureTSLanguageServer()
	requireBinary(t, "typescript-language-server")
	src := "export function add(x: number, y: number): number { return x + y }"
	syms, err := ParseText("typescript-language-server", []string{"--stdio"}, "typescript", src)
	if err != nil {
		t.Fatalf("parse ts: %v", err)
	}
	if len(syms) == 0 {
		t.Fatalf("expected symbols")
	}
}
