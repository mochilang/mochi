//go:build slow

package erlcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	erlcode "mochi/compile/x/erlang"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestErlangCompiler_TwoSum(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
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
	c := erlcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.erl")
	if err := os.WriteFile(file, code, 0755); err != nil {
		t.Fatalf("write error: %v", err)
	}
	out, err := exec.Command("escript", file).CombinedOutput()
	if err != nil {
		t.Fatalf("escript error: %v\n%s", err, out)
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	var filtered []string
	for _, line := range lines {
		if strings.Contains(line, "Warning") || strings.HasPrefix(line, "/tmp") {
			continue
		}
		filtered = append(filtered, line)
	}
	got := strings.Join(filtered, "\n")
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestErlangCompiler_LeetCode1(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	runLeetExample(t, 1)
}

// runLeetExample compiles and executes all Mochi programs under
// examples/leetcode/<id>. It fails the test if compilation or execution fails.
func runLeetExample(t *testing.T, id int) {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
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
			c := erlcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.erl")
			if err := os.WriteFile(file, code, 0755); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("escript", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("escript error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

func TestErlangCompiler_LeetCodeExamples(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	// Only the two-sum example currently compiles successfully.
	// Later LeetCode problems rely on language features
	// not yet supported by the Erlang backend.
	runLeetExample(t, 1)
}

func TestErlangCompiler_SubsetPrograms(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}

	// Provide a fake erlfmt to avoid network installation attempts.
	tmpFmt := t.TempDir()
	fake := filepath.Join(tmpFmt, "erlfmt")
	if err := os.WriteFile(fake, []byte("#!/bin/sh\ncat"), 0755); err == nil {
		oldPath := os.Getenv("PATH")
		os.Setenv("PATH", tmpFmt+":"+oldPath)
		t.Cleanup(func() { os.Setenv("PATH", oldPath) })
	}

	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}

		// Execute using the VM to obtain expected output.
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var wantBuf bytes.Buffer
		m := vm.New(p, &wantBuf)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("❌ vm run error: %w", err)
		}
		want := strings.TrimSpace(wantBuf.String())

		// Compile to Erlang and run with escript.
		code, err := erlcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.erl")
		if err := os.WriteFile(file, code, 0755); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("escript", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ escript error: %w\n%s", err, out)
		}
		got := strings.TrimSpace(string(out))
		if got != want {
			return nil, fmt.Errorf("❌ output mismatch:\n-- got --\n%s\n-- want --\n%s", got, want)
		}
		return []byte(got), nil
	}

	golden.Run(t, "tests/compiler/erl_simple", ".mochi", ".out", run)

	golden.Run(t, "tests/compiler/erl", ".mochi", ".out", run)
}

func TestErlangCompiler_GoldenOutput(t *testing.T) {
	tmpFmt := t.TempDir()
	fake := filepath.Join(tmpFmt, "erlfmt")
	if err := os.WriteFile(fake, []byte("#!/bin/sh\ncat"), 0755); err == nil {
		oldPath := os.Getenv("PATH")
		os.Setenv("PATH", tmpFmt+":"+oldPath)
		t.Cleanup(func() { os.Setenv("PATH", oldPath) })
	}
	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := erlcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/erl_simple", ".mochi", ".erl.out", run)
	golden.Run(t, "tests/compiler/erl", ".mochi", ".erl.out", run)
}
