package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rbcode "mochi/compile/rb"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_TwoSum(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
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
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rb")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("ruby", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("ruby run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestRBCompiler_SubsetPrograms(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/rb", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		code, err := rbcode.New(env).Compile(prog)
		if err != nil {
			return nil, err
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.rb")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("ruby", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("ruby run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}
