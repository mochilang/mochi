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
	cmd := exec.Command("chibi-scheme", file)
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
		cmd := exec.Command("chibi-scheme", file)
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
