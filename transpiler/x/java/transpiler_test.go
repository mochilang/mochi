//go:build slow

package javatr_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	javatr "mochi/transpiler/x/java"
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
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "print_hello.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "print_hello.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileLetAndPrint(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "let_and_print.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "let_and_print.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "let_and_print.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "let_and_print.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "let_and_print.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileBasicCompare(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "basic_compare.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "basic_compare.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "basic_compare.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "basic_compare.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "basic_compare.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileUnaryNeg(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "unary_neg.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "unary_neg.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "unary_neg.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "unary_neg.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "unary_neg.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileVarAssignment(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "var_assignment.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "var_assignment.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "var_assignment.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "var_assignment.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "var_assignment.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileWhileLoop(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "while_loop.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "while_loop.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "while_loop.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "while_loop.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "while_loop.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileLenString(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "len_string.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "len_string.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "len_string.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "len_string.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "len_string.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileStringConcat(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "java")
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
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	javaPath := filepath.Join(outDir, "string_concat.java")
	if err := os.WriteFile(javaPath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	mainPath := filepath.Join(outDir, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("write main: %v", err)
	}

	cmd := exec.Command("javac", "Main.java")
	cmd.Dir = outDir
	if out, err := cmd.CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "string_concat.error"), out, 0o644)
		t.Fatalf("javac error: %v", err)
	}

	cmd = exec.Command("java", "-cp", outDir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, "string_concat.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, "string_concat.error"))
	got := bytes.TrimSpace(out)
	want, _ := os.ReadFile(filepath.Join(outDir, "string_concat.out"))
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}
