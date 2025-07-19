package scheme_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	scheme "mochi/transpiler/x/scheme"
	"mochi/types"
)

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func ensureScheme() (string, error) {
	return exec.LookPath("chibi-scheme")
}

func TestSchemeTranspiler_PrintHello(t *testing.T) {
	schemePath, err := ensureScheme()
	if err != nil {
		t.Skipf("scheme not installed: %v", err)
	}

	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "scheme")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := scheme.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := scheme.Format(scheme.EmitString(ast))
	scmPath := filepath.Join(outDir, "print_hello.scm")
	if err := os.WriteFile(scmPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	cmd := exec.Command(schemePath, "-m", "chibi", scmPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "print_hello.error"))
	got := bytes.TrimSpace(out)
	wantPath := filepath.Join(outDir, "print_hello.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}
