//go:build slow

package cobol_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	cobol "mochi/transpiler/x/cobol"
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

func runFile(t *testing.T, name string) {
	if err := cobol.EnsureCOBOL(); err != nil {
		t.Skip("cobc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
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
	ast, err := cobol.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := cobol.Emit(ast)
	cobPath := filepath.Join(outDir, name+".cob")
	if err := os.WriteFile(cobPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command("cobc", "-free", cobPath, "-x", "-o", exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("compile: %v", err)
	}
	defer os.Remove(exe)
	out, err := exec.Command(exe).CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	wantPath := filepath.Join(outDir, name+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_PrintHello(t *testing.T)    { runFile(t, "print_hello") }
func TestTranspile_LetAndPrint(t *testing.T)   { runFile(t, "let_and_print") }
func TestTranspile_TypedLet(t *testing.T)      { runFile(t, "typed_let") }
func TestTranspile_TypedVar(t *testing.T)      { runFile(t, "typed_var") }
func TestTranspile_UnaryNeg(t *testing.T)      { runFile(t, "unary_neg") }
func TestTranspile_VarAssignment(t *testing.T) { runFile(t, "var_assignment") }
func TestTranspile_MathOps(t *testing.T)       { runFile(t, "math_ops") }
