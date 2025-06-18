//go:build slow

package schemecode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	schemecode "mochi/compile/scheme"
	"mochi/parser"
	"mochi/types"
)

// TestSchemeCompiler_TwoSum compiles the LeetCode example to Scheme and runs it.
func TestSchemeCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("chibi-scheme"); err != nil {
		t.Skip("chibi-scheme not installed")
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
