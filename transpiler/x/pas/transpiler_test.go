//go:build slow

package pas_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pascode "mochi/compiler/x/pascal"
	"mochi/parser"
	pas "mochi/transpiler/x/pas"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func runCase(t *testing.T, name string) {
	t.Helper()
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Skip("fpc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pas")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := pas.Transpile(env, prog)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	pasPath := filepath.Join(outDir, name+".pas")
	if err := os.WriteFile(pasPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	exe := filepath.Join(outDir, name)
	cmd := exec.Command(fpc, pasPath, "-o"+exe)
	if out, err := cmd.CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("compile: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("%s output mismatch: got %s want %s", name, got, want)
	}
}

func TestPascalTranspiler(t *testing.T) {
	for _, tc := range []string{"print_hello", "unary_neg", "math_ops", "let_and_print", "var_assignment", "typed_let", "typed_var"} {
		t.Run(tc, func(t *testing.T) { runCase(t, tc) })
	}
}
