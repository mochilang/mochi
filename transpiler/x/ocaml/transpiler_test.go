//go:build slow && !rosetta

package ocaml_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	"mochi/parser"
	ocaml "mochi/transpiler/x/ocaml"
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
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
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
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "print_hello.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "print_hello")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileBasicCompare(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "basic_compare.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "basic_compare.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "basic_compare")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "basic_compare.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "basic_compare.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileLetAndPrint(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "let_and_print.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "let_and_print.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "let_and_print")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "let_and_print.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "let_and_print.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMathOps(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "math_ops.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "math_ops.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "math_ops")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "math_ops.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "math_ops.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileStringConcat(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "string_concat.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "string_concat.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "string_concat")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "string_concat.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "string_concat.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileStringCompare(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "string_compare.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "string_compare.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "string_compare")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "string_compare.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "string_compare.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileIfThenElse(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "if_then_else.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "if_then_else.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "if_then_else")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "if_then_else.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "if_then_else.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileUnaryNeg(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "unary_neg.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "unary_neg.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "unary_neg")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "unary_neg.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "unary_neg.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileBinaryPrecedence(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "binary_precedence.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "binary_precedence.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "binary_precedence")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "binary_precedence.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "binary_precedence.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileIfThenElseNested(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "if_then_else_nested.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "if_then_else_nested.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "if_then_else_nested")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "if_then_else_nested.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "if_then_else_nested.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileStrBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "str_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "str_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "str_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "str_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "str_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMapIndex(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "map_index.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "map_index.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "map_index")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "map_index.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "map_index.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMapAssign(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "map_assign.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "map_assign.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "map_assign")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "map_assign.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "map_assign.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileTypedLet(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "typed_let.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "typed_let.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "typed_let")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "typed_let.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "typed_let.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileTypedVar(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "typed_var.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "typed_var.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "typed_var")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "typed_var.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "typed_var.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileVarAssignment(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "var_assignment.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "var_assignment.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "var_assignment")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "var_assignment.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "var_assignment.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileLenString(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "len_string.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "len_string.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "len_string")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "len_string.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "len_string.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileIfElse(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "if_else.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "if_else.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "if_else")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "if_else.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "if_else.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileWhileLoop(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "while_loop.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "while_loop.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "while_loop")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "while_loop.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "while_loop.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileSubstringBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "substring_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "substring_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "substring_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "substring_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "substring_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileSumBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "sum_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "sum_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "sum_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "sum_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "sum_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileForLoop(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "for_loop.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "for_loop.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "for_loop")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "for_loop.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "for_loop.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileForListCollection(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "for_list_collection.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "for_list_collection.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "for_list_collection")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "for_list_collection.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "for_list_collection.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileStringContains(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "string_contains.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "string_contains.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "string_contains")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "string_contains.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "string_contains.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileBoolChain(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "bool_chain.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "bool_chain.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "bool_chain")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "bool_chain.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "bool_chain.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileListIndex(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "list_index.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "list_index.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "list_index")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "list_index.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "list_index.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileStringIndex(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "string_index.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "string_index.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "string_index")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "string_index.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "string_index.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileAppendBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "append_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "append_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "append_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "append_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "append_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileAvgBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "avg_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "avg_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "avg_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "avg_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "avg_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileCountBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "count_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "count_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "count_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "count_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "count_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileFunCall(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "fun_call.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "fun_call.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "fun_call")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "fun_call.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "fun_call.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileCastStringToInt(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "cast_string_to_int.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "cast_string_to_int.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "cast_string_to_int")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "cast_string_to_int.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "cast_string_to_int.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileListNestedAssign(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "list_nested_assign.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "list_nested_assign.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "list_nested_assign")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "list_nested_assign.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "list_nested_assign.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileFunExprInLet(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "fun_expr_in_let.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "fun_expr_in_let.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "fun_expr_in_let")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "fun_expr_in_let.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "fun_expr_in_let.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileFunThreeArgs(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "fun_three_args.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "fun_three_args.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "fun_three_args")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "fun_three_args.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "fun_three_args.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMembership(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "membership.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "membership.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "membership")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "membership.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "membership.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMinMaxBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "min_max_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "min_max_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "min_max_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "min_max_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "min_max_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileValuesBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "values_builtin.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "values_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "values_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "values_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "values_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileForMapCollection(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "for_map_collection.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "for_map_collection.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "for_map_collection")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "for_map_collection.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "for_map_collection.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMapIntKey(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "map_int_key.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "map_int_key.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "map_int_key")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "map_int_key.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "map_int_key.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileMapNestedAssign(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "map_nested_assign.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "map_nested_assign.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "map_nested_assign")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "map_nested_assign.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "map_nested_assign.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileNestedFunction(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "nested_function.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "nested_function.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "nested_function")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "nested_function.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "nested_function.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileStringPrefixSlice(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "string_prefix_slice.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "string_prefix_slice.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "string_prefix_slice")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "string_prefix_slice.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "string_prefix_slice.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileTailRecursion(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "tail_recursion.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "tail_recursion.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "tail_recursion")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "tail_recursion.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "tail_recursion.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileCrossJoin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "cross_join.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "cross_join.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "cross_join")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "cross_join.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "cross_join.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspileDatasetWhereFilter(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "dataset_where_filter.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	mlFile := filepath.Join(outDir, "dataset_where_filter.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "dataset_where_filter")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "dataset_where_filter.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "dataset_where_filter.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestOCamlTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".ml")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		ast, err := ocaml.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(outDir, base)
		if out, err := exec.Command("ocamlc", codePath, "-o", exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(out)
		_ = os.WriteFile(outPath, outBytes, 0o644)
		_ = os.Remove(errPath)
		return outBytes, nil
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	readmePath := filepath.Join(root, "transpiler", "x", "ocaml", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".ml")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s.mochi", i+1, mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi OCaml Transpiler\n\n")
	buf.WriteString("This folder contains an experimental transpiler that converts Mochi source code into OCaml.\n\n")
	fmt.Fprintf(&buf, "## Golden Test Checklist (%d/%d)\n\n", compiled, total)
	buf.WriteString("The list below tracks Mochi programs under `tests/vm/valid` that should successfully transpile. Checked items indicate tests known to work.\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			buf.WriteString("\nLast updated " + t.Format("2006-01-02 15:04 MST") + "\n")
		}
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "ocaml", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%h%n%cI%n%s").Output()
	var hash, ts, msg string
	if err == nil {
		parts := strings.SplitN(strings.TrimSpace(string(out)), "\n", 3)
		if len(parts) == 3 {
			hash, ts, msg = parts[0], parts[1], parts[2]
		}
	}
	if t, perr := time.Parse(time.RFC3339, ts); perr == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}

	data, _ := os.ReadFile(taskFile)
	var keep []string
	for _, line := range strings.Split(string(data), "\n") {
		if strings.HasPrefix(line, "## Progress") || strings.HasPrefix(line, "- VM valid") {
			continue
		}
		keep = append(keep, line)
	}

	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		} else if _, err := os.Stat(filepath.Join(outDir, name+".ml")); err == nil {
			compiled++
		}
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	if msg == "" {
		buf.WriteString("- VM valid golden test results updated\n\n")
	} else {
		buf.WriteString(fmt.Sprintf("- %s (%s)\n\n", msg, hash))
	}
	fmt.Fprintf(&buf, "- VM valid programs compiled: %d/%d\n\n", compiled, total)
	buf.WriteString(strings.Join(keep, "\n"))
	if len(keep) > 0 && keep[len(keep)-1] != "" {
		buf.WriteString("\n")
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
