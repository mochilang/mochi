//go:build slow

package ocaml_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

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
