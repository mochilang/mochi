package ex_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	ex "mochi/transpiler/x/ex"
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

func TestTranspile_Golden(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	os.MkdirAll(outDir, 0o755)

	cases := []string{
		"print_hello",
		"binary_precedence",
		"cast_string_to_int",
		"count_builtin",
		"len_builtin",
		"len_string",
		"math_ops",
		"sum_builtin",
		"avg_builtin",
		"unary_neg",
	}
	for _, base := range cases {
		src := filepath.Join(srcDir, base+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse %s: %v", base, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type %s: %v", base, errs[0])
		}
		ast, err := ex.Transpile(prog, env)
		if err != nil {
			t.Fatalf("transpile %s: %v", base, err)
		}
		code := ex.Emit(ast)
		file := filepath.Join(outDir, base+".exs")
		if err := os.WriteFile(file, code, 0o644); err != nil {
			t.Fatalf("write %s: %v", base, err)
		}
		cmd := exec.Command("elixir", file)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
			t.Fatalf("run %s: %v", base, err)
		}
		_ = os.Remove(filepath.Join(outDir, base+".error"))
		wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
		if err != nil {
			t.Fatalf("read want %s: %v", base, err)
		}
		want := bytes.TrimSpace(wantBytes)
		if !bytes.Equal(got, want) {
			t.Errorf("%s: output mismatch\nGot: %s\nWant: %s", base, got, want)
		}
	}
}
