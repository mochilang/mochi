//go:build slow

package pl_test

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

func TestTranspile_LenString(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "len_string.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "len_string.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "len_string.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "len_string.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_LenBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "len_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "len_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "len_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "len_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_StringConcat(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "string_concat.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "string_concat.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "string_concat.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "string_concat.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_StrBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "str_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "str_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "str_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "str_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_StringCompare(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "string_compare.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "string_compare.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "string_compare.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "string_compare.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_AppendBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "append_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "append_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "append_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "append_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_CountBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "count_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "count_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "count_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "count_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_SumBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "sum_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "sum_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "sum_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "sum_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_MinMaxBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "min_max_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "min_max_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "min_max_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "min_max_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_AvgBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "avg_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "avg_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "avg_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "avg_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_SubstringBuiltin(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "substring_builtin.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "substring_builtin.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "substring_builtin.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "substring_builtin.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_ListIndex(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "list_index.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "list_index.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "list_index.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "list_index.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_IfThenElse(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "if_then_else.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "if_then_else.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "if_then_else.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "if_then_else.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_IfThenElseNested(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "if_then_else_nested.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "if_then_else_nested.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "if_then_else_nested.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "if_then_else_nested.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_StringIndex(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	plFile := filepath.Join(outDir, "string_index.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "string_index.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "string_index.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "string_index.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_ListAssign(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "list_assign.mochi")
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
	plFile := filepath.Join(outDir, "list_assign.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "list_assign.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "list_assign.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "list_assign.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspile_GroupBy(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "group_by.mochi")
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
	plFile := filepath.Join(outDir, "group_by.pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "group_by.error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "group_by.error"))
	want, err := os.ReadFile(filepath.Join(outDir, "group_by.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func countCompiled() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	readme := filepath.Join(root, "transpiler", "x", "pl", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s `%s`", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Prolog Transpiler\n\n")
	buf.WriteString("This directory contains a tiny transpiler that converts a restricted subset of Mochi programs to SWI-Prolog. It is mainly used for experimentation and golden tests.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n*Checklist generated automatically from tests/vm/valid*")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "pl", "TASKS.md")
	compiled, total := countCompiled()
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString(fmt.Sprintf("- VM valid golden test results updated to %d/%d\n\n", compiled, total))
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
