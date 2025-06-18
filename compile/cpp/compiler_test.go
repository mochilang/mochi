//go:build slow

package cppcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cppcode "mochi/compile/cpp"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestCPPCompiler_TwoSum compiles the LeetCode example to C++ and runs it.
func TestCPPCompiler_TwoSum(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
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
	c := cppcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "prog.cpp")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	bin := filepath.Join(dir, "prog")
	if out, err := exec.Command(cpp, file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("cpp error: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := string(bytes.TrimSpace(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}

func TestCPPCompiler_SubsetPrograms(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/cpp", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := cppcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.cpp")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		bin := filepath.Join(dir, "prog")
		if out, err := exec.Command(cpp, file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ cpp error: %w\n%s", err, out)
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestCPPCompiler_GoldenOutput(t *testing.T) {
	if _, err := cppcode.EnsureCPP(); err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".cpp.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := cppcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})

	golden.Run(t, "tests/compiler/cpp", ".mochi", ".cpp.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := cppcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
