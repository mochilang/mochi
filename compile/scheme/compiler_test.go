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

func runLeetExample(t *testing.T, id int) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
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
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.scm")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("chibi-scheme", "-m", "chibi", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("scheme run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

// TestSchemeCompiler_LeetCode1 compiles the two-sum example and runs it.
func TestSchemeCompiler_LeetCode1(t *testing.T) {
	runLeetExample(t, 1)
}

// TestSchemeCompiler_LeetCode2 compiles the add-two-numbers example and runs it.
func TestSchemeCompiler_LeetCode2(t *testing.T) {
	runLeetExample(t, 2)
}

// TestSchemeCompiler_LeetCode3 compiles the longest-substring example and runs it.
func TestSchemeCompiler_LeetCode3(t *testing.T) {
	runLeetExample(t, 3)
}

// TestSchemeCompiler_LeetCode4 compiles the median-of-two-sorted-arrays example and runs it.
func TestSchemeCompiler_LeetCode4(t *testing.T) {
	runLeetExample(t, 4)
}

// TestSchemeCompiler_LeetCode5 compiles the longest-palindromic-substring example and runs it.
func TestSchemeCompiler_LeetCode5(t *testing.T) {
	runLeetExample(t, 5)
}

func TestSchemeCompiler_SubsetPrograms(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
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
