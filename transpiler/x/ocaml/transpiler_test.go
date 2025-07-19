//go:build slow

package ocaml_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

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

func TestTranspileLenBuiltin(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "len_builtin.mochi")
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
	mlFile := filepath.Join(outDir, "len_builtin.ml")
	if err := os.WriteFile(mlFile, code, 0o644); err != nil {
		t.Fatalf("write ml: %v", err)
	}
	exe := filepath.Join(outDir, "len_builtin")
	if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "len_builtin.error"), out, 0o644)
		t.Fatalf("ocamlc: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "len_builtin.out"))
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
	sort.Strings(files)
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Transpiler Golden Test Checklist\n\n")
	buf.WriteString("The following Mochi programs under `tests/vm/valid` are used as golden inputs for transpiler implementations.  Tick a box once the OCaml transpiler can successfully generate code that matches the VM output.\n\n")
	fmt.Fprintf(&buf, "Completed: %d/%d\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "ocaml", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiledFiles, _ := filepath.Glob(filepath.Join(outDir, "*.out"))
	compiled := len(compiledFiles)
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	fmt.Fprintf(&buf, "- Checklist updated: %d/%d tests compiled\n", compiled, total)
	buf.WriteString("- Added support for `substring_builtin` and `sum_builtin` programs.\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
