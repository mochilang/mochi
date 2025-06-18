//go:build slow

package schemecode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	schemecode "mochi/compile/scheme"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestSchemeCompiler_TwoSum compiles the LeetCode example to Scheme and runs it.
func TestSchemeCompiler_TwoSum(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := schemecode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("scheme run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	if string(got) != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestSchemeCompiler_SubsetPrograms(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}

	files := []string{
		"for_loop",
		"fun_call",
		"grouped_expr",
		"if_else",
		"len_builtin",
		"let_and_print",
		"list_index",
		"print_hello",
		"two_sum",
		"var_assignment",
		"while_loop",
	}
	for _, name := range files {
		src := filepath.Join("tests", "compiler", "valid", name+".mochi")
		runSchemeProgram(t, src, ".out")
	}

	golden.Run(t, "tests/compiler/scheme", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := schemecode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.scm")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c scheme run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestSchemeCompiler_GoldenOutput(t *testing.T) {
	files := []string{
		"for_loop",
		"fun_call",
		"grouped_expr",
		"if_else",
		"len_builtin",
		"let_and_print",
		"list_index",
		"print_hello",
		"two_sum",
		"var_assignment",
		"while_loop",
	}
	for _, name := range files {
		src := filepath.Join("tests", "compiler", "valid", name+".mochi")
		runSchemeGolden(t, src, ".scm.out")
	}

	golden.Run(t, "tests/compiler/scheme", ".mochi", ".scm.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := schemecode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func runSchemeProgram(t *testing.T, src, goldenExt string) {
	t.Helper()
	root := findRoot(t)
	path := filepath.Join(root, src)
	name := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	wantPath := filepath.Join(root, filepath.Dir(src), name+goldenExt)

	prog, err := parser.Parse(path)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := schemecode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.scm")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
	if data, err := os.ReadFile(strings.TrimSuffix(path, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("scheme run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("failed to read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+goldenExt, got, want)
	}
}

func runSchemeGolden(t *testing.T, src, goldenExt string) {
	t.Helper()
	root := findRoot(t)
	path := filepath.Join(root, src)
	name := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	wantPath := filepath.Join(root, filepath.Dir(src), name+goldenExt)

	prog, err := parser.Parse(path)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := schemecode.New(env)
	got, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	got = bytes.TrimSpace(got)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("failed to read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+goldenExt, got, want)
	}
}

func findRoot(t *testing.T) string {
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
