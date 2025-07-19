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

func TestTranspile_Golden(t *testing.T) {
	if err := cobol.EnsureCOBOL(); err != nil {
		t.Skip("cobc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
	os.MkdirAll(outDir, 0o755)

	cases := []string{
		"print_hello",
		"string_concat",
		"unary_neg",
		"var_assignment",
		"typed_let",
		"typed_var",
		"let_and_print",
		"if_else",
	}
	for _, name := range cases {
		src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type %s: %v", name, errs[0])
		}
		ast, err := cobol.Transpile(prog, env)
		if err != nil {
			t.Fatalf("transpile %s: %v", name, err)
		}
		code := cobol.Emit(ast)
		cobPath := filepath.Join(outDir, name+".cob")
		if err := os.WriteFile(cobPath, code, 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
		exe := filepath.Join(outDir, name)
		if out, err := exec.Command("cobc", "-free", cobPath, "-x", "-o", exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("compile %s: %v", name, err)
		}
		out, err := exec.Command(exe).CombinedOutput()
		_ = os.Remove(exe)
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("run %s: %v", name, err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		wantPath := filepath.Join(outDir, name+".out")
		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Fatalf("read want %s: %v", name, err)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("%s output mismatch:\nGot: %s\nWant: %s", name, got, want)
		}
	}
}
