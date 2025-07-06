//go:build slow

package plcode_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	plcode "mochi/compile/x/pl"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestPrologCompiler_LeetCode1(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
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
	c := plcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.pl")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("swipl", "-q", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("swipl error: %v\n%s", err, out)
	}
	expected := "0\n1"
	if strings.TrimSpace(string(out)) != expected {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestPrologCompiler_LeetCode2(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	t.Skip("LeetCode examples beyond #1 are not fully supported")
	src := filepath.Join("..", "..", "..", "examples", "leetcode", "2", "add-two-numbers.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := plcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.pl")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("swipl", "-q", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("swipl error: %v\n%s", err, out)
	}
	if strings.TrimSpace(string(out)) != "" {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestPrologCompiler_LeetCode3(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	t.Skip("LeetCode examples beyond #1 are not fully supported")
	src := filepath.Join("..", "..", "..", "examples", "leetcode", "3", "longest-substring-without-repeating-characters.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := plcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.pl")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("swipl", "-q", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("swipl error: %v\n%s", err, out)
	}
	if strings.TrimSpace(string(out)) != "" {
		t.Fatalf("unexpected output: %q", out)
	}
}

// runLeetExample compiles the Mochi solution for the given LeetCode ID to
// Prolog and executes the generated program. The test fails if compilation or
// execution returns an error.
func runLeetExample(t *testing.T, id int) {
	dir := filepath.Join("..", "..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := plcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.pl")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("swipl", "-q", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("swipl error: %v\n%s", err, out)
			}
		})
	}
}

func TestPrologCompiler_SubsetPrograms(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/pl", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := plcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.pl")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("swipl", "-q", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c swipl error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestPrologCompiler_LeetCodeExamples(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	t.Skip("LeetCode examples beyond #1 are not fully supported")
	for i := 1; i <= 10; i++ {
		runLeetExample(t, i)
	}
}

func fileExists(path string) bool {
	if _, err := os.Stat(path); err == nil {
		return true
	}
	return false
}

func TestPrologCompiler_GoldenOutput(t *testing.T) {
	compileFn := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := plcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}

		// Run compiled code with SWI-Prolog
		dir, err := os.MkdirTemp("", "mochi-pl")
		if err != nil {
			return nil, err
		}
		defer os.RemoveAll(dir)
		file := filepath.Join(dir, "main.pl")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("swipl", "-q", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c swipl error: %w\n%s", err, out)
		}
		plOut := strings.TrimSpace(string(out))

		// Run program using Mochi VM for comparison
		srcData, _ := os.ReadFile(src)
		p, err := vm.CompileWithSource(prog, env, string(srcData))
		if err != nil {
			return nil, fmt.Errorf("\u274c vm compile error: %w", err)
		}
		var in io.Reader = os.Stdin
		if fileExists(strings.TrimSuffix(src, ".mochi") + ".in") {
			f, err := os.Open(strings.TrimSuffix(src, ".mochi") + ".in")
			if err == nil {
				defer f.Close()
				in = f
			}
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, in, &vmBuf)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("\u274c vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("\u274c vm run error: %v", err)
		}
		vmOut := strings.TrimSpace(vmBuf.String())
		if plOut != vmOut {
			return nil, fmt.Errorf("output mismatch\n-- pl --\n%s\n-- vm --\n%s", plOut, vmOut)
		}

		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/pl", ".mochi", ".pl.out", compileFn)
	golden.Run(t, "tests/compiler/valid", ".mochi", ".pl.out", compileFn)
}
