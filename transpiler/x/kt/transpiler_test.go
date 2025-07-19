package kt_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	kt "mochi/transpiler/x/kt"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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

func TestTranspilePrintHello(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
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
	ast, err := kt.Transpile(env, prog)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := kt.Emit(ast)
	ktFile := filepath.Join(outDir, "print_hello.kt")
	if err := os.WriteFile(ktFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	jar := filepath.Join(outDir, "print_hello.jar")
	if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("kotlinc: %v", err)
	}
	cmd := exec.Command("java", "-jar", jar)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "print_hello.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}
