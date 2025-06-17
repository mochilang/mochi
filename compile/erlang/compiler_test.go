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

	erlcode "mochi/compile/erlang"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestErlangCompiler_LeetCode1(t *testing.T) {
	t.Skip("disabled in current environment")
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
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
	cmd := exec.Command("escript", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("escript error: %v\n%s", err, out)
	}
	expected := "0\n1\n"
	if string(out) != expected {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestErlangCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	golden.Run(t, "tests/compiler/erl_simple", ".mochi", ".out", func(src string) ([]byte, error) {
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
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestErlangCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/erl_simple", ".mochi", ".erl.out", func(src string) ([]byte, error) {
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
	})
}
