//go:build slow

package rs_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	rs "mochi/transpiler/x/rs"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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

func TestTranspile_PrintHello(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
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
	progAST, err := rs.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := rs.Emit(progAST)
	rsFile := filepath.Join(outDir, "print_hello.rs")
	if err := os.WriteFile(rsFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	tmp := t.TempDir()
	bin := filepath.Join(tmp, "print_hello")
	if out, err := exec.Command("rustc", rsFile, "-O", "-o", bin).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("rustc: %v", err)
	}
	out, err := exec.Command(bin).CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "print_hello.error"))
	wantPath := filepath.Join(outDir, "print_hello.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_LetAndPrint(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
	os.MkdirAll(outDir, 0o755)

	base := "let_and_print"
	src := filepath.Join(root, "tests", "vm", "valid", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	progAST, err := rs.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := rs.Emit(progAST)
	rsFile := filepath.Join(outDir, base+".rs")
	if err := os.WriteFile(rsFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	tmp := t.TempDir()
	bin := filepath.Join(tmp, base)
	if out, err := exec.Command("rustc", rsFile, "-O", "-o", bin).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
		t.Fatalf("rustc: %v", err)
	}
	out, err := exec.Command(bin).CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
	wantPath := filepath.Join(outDir, base+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func runExample(t *testing.T, base string) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	progAST, err := rs.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := rs.Emit(progAST)
	rsFile := filepath.Join(outDir, base+".rs")
	if err := os.WriteFile(rsFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	tmp := t.TempDir()
	bin := filepath.Join(tmp, base)
	if out, err := exec.Command("rustc", rsFile, "-O", "-o", bin).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
		t.Fatalf("rustc: %v", err)
	}
	out, err := exec.Command(bin).CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
	wantPath := filepath.Join(outDir, base+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_TypedLet(t *testing.T)         { runExample(t, "typed_let") }
func TestTranspile_TypedVar(t *testing.T)         { runExample(t, "typed_var") }
func TestTranspile_VarAssignment(t *testing.T)    { runExample(t, "var_assignment") }
func TestTranspile_UnaryNeg(t *testing.T)         { runExample(t, "unary_neg") }
func TestTranspile_BasicCompare(t *testing.T)     { runExample(t, "basic_compare") }
func TestTranspile_IfElse(t *testing.T)           { runExample(t, "if_else") }
func TestTranspile_IfThenElse(t *testing.T)       { runExample(t, "if_then_else") }
func TestTranspile_IfThenElseNested(t *testing.T) { runExample(t, "if_then_else_nested") }
