//go:build slow

package pl_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	pl "mochi/transpiler/x/pl"
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
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	code := buf.Bytes()
	plFile := filepath.Join(outDir, "print_hello.pl")
	if err := os.WriteFile(plFile, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
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

func TestTranspile_BasicCompare(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "basic_compare.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "basic_compare.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "basic_compare.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "basic_compare.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_BinaryPrecedence(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "binary_precedence.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "binary_precedence.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "binary_precedence.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "binary_precedence.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_CastStringToInt(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "cast_string_to_int.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "cast_string_to_int.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "cast_string_to_int.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "cast_string_to_int.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_TypedLet(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "typed_let.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "typed_let.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "typed_let.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "typed_let.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_TypedVar(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "typed_var.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "typed_var.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "typed_var.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "typed_var.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_VarAssignment(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "var_assignment.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "var_assignment.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "var_assignment.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "var_assignment.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_UnaryNeg(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "unary_neg.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "unary_neg.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "unary_neg.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "unary_neg.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_MathOps(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "math_ops.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "math_ops.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "math_ops.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "math_ops.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_LetAndPrint(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "let_and_print.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "let_and_print.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "let_and_print.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "let_and_print.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}
