//go:build slow

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

func TestTranspilePrograms(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	cases := []string{
		"print_hello",
		"count_builtin",
		"avg_builtin",
		"sum_builtin",
		"len_builtin",
		"len_string",
		"str_builtin",
		"let_and_print",
		"basic_compare",
		"binary_precedence",
		"math_ops",
		"unary_neg",
		"string_compare",
		"string_concat",
		"append_builtin",
		"min_max_builtin",
		"substring_builtin",
		"typed_let",
		"typed_var",
		"var_assignment",
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	os.MkdirAll(outDir, 0o755)
	for _, name := range cases {
		t.Run(name, func(t *testing.T) {
			src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
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
			ktFile := filepath.Join(outDir, name+".kt")
			if err := os.WriteFile(ktFile, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			jar := filepath.Join(outDir, name+".jar")
			if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
				t.Fatalf("kotlinc: %v", err)
			}
			cmd := exec.Command("java", "-jar", jar)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
			want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
			}
		})
	}
}
