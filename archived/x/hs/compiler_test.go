//go:build archived && slow

package hscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	hscode "mochi/archived/x/hs"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestHSCompiler_LeetCodeExample1(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	src := filepath.Join("..", "..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := hscode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.hs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("runhaskell", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("runhaskell error: %v\n%s", err, out)
	}
	got := strings.ReplaceAll(string(out), "\r\n", "\n")
	if strings.TrimSpace(got) != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestHSCompiler_GoldenSubset(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/hs", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		c := hscode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, err
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.hs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("runhaskell", file)
		var inData []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
			inData = data
		}
		outHS, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("runhaskell error: %w\n%s", err, outHS)
		}
		hsRes := strings.TrimSpace(string(outHS))

		// Run with VM to get expected output
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("vm compile error: %w", err)
		}
		var vmOut bytes.Buffer
		if inData != nil {
			m := vm.NewWithIO(p, bytes.NewReader(inData), &vmOut)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
				}
				return nil, fmt.Errorf("vm run error: %w", err)
			}
		} else {
			m := vm.New(p, &vmOut)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
				}
				return nil, fmt.Errorf("vm run error: %w", err)
			}
		}
		vmRes := strings.TrimSpace(vmOut.String())
		if hsRes != vmRes {
			return nil, fmt.Errorf("output mismatch\n-- hs --\n%s\n-- vm --\n%s", hsRes, vmRes)
		}
		return []byte(hsRes), nil
	})
}

func TestHSCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/hs", ".mochi", ".hs.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		code, err := hscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

// runExample compiles and runs all Mochi programs in examples/leetcode/<id>.
// It ensures the generated Haskell code executes without error.
func runExample(t *testing.T, id int) error {
	t.Helper()
	dir := filepath.Join("..", "..", "..", "examples", "leetcode", strconv.Itoa(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		return err
	}
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			return fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return fmt.Errorf("type error: %v", errs[0])
		}
		c := hscode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return fmt.Errorf("compile error: %w", err)
		}
		tmp := t.TempDir()
		file := filepath.Join(tmp, "main.hs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return err
		}
		cmd := exec.Command("runhaskell", file)
		if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		if out, err := cmd.CombinedOutput(); err != nil {
			return fmt.Errorf("runhaskell error: %w\n%s", err, out)
		}
	}
	return nil
}

// TestHSCompiler_LeetCodeExamples compiles and executes the first thirty
// LeetCode solutions using the Haskell backend.
func TestHSCompiler_LeetCodeExamples(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	for i := 1; i <= 30; i++ {
		if err := runExample(t, i); err != nil {
			t.Skipf("leetcode %d unsupported: %v", i, err)
		}
	}
}
