package erl_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	erl "mochi/transpiler/x/erl"
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

func runGolden(t *testing.T, name string) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "erl")
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
	ast, err := erl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	erlFile := filepath.Join(outDir, name+".erl")
	if err := os.WriteFile(erlFile, code, 0o755); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("escript", erlFile)
	out, err := cmd.CombinedOutput()
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

func TestTranspilePrintHello(t *testing.T) { runGolden(t, "print_hello") }

func TestTranspileUnaryNeg(t *testing.T) { runGolden(t, "unary_neg") }

func TestTranspileStringCompare(t *testing.T) { runGolden(t, "string_compare") }

func TestTranspileLenString(t *testing.T) { runGolden(t, "len_string") }

func TestTranspileStrBuiltin(t *testing.T) { runGolden(t, "str_builtin") }

func TestTranspileLenBuiltin(t *testing.T) { runGolden(t, "len_builtin") }

func TestTranspileLetAndPrint(t *testing.T) { runGolden(t, "let_and_print") }

func TestTranspileStringConcat(t *testing.T) { runGolden(t, "string_concat") }

func TestTranspileIfThenElse(t *testing.T) { runGolden(t, "if_then_else") }

func TestTranspileIfThenElseNested(t *testing.T) { runGolden(t, "if_then_else_nested") }

func TestTranspileAppendBuiltin(t *testing.T) { runGolden(t, "append_builtin") }

func TestTranspileAvgBuiltin(t *testing.T) { runGolden(t, "avg_builtin") }

func TestTranspileCountBuiltin(t *testing.T) { runGolden(t, "count_builtin") }

func TestTranspileSumBuiltin(t *testing.T) { runGolden(t, "sum_builtin") }

func TestTranspileMinMaxBuiltin(t *testing.T) { runGolden(t, "min_max_builtin") }
