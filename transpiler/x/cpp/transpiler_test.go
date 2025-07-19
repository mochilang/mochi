//go:build slow

package cpp_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/compiler/x/testutil"
	"mochi/parser"
	cpp "mochi/transpiler/x/cpp"
	"mochi/types"
)

func TestCPPTranspiler_PrintHello(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cpp")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	ast, err := cpp.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile error: %v", err)
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, "print_hello.cpp")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(outDir, "print_hello")
	if out, err := exec.Command("g++", codePath, "-std=c++20", "-o", bin).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("compile error: %v", err)
	}
	defer os.Remove(bin)
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	got := bytes.TrimSpace(out)
       want, _ := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}
