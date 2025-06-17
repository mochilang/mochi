package hscode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	hscode "mochi/compile/hs"
	"mochi/parser"
	"mochi/types"
)

func TestHaskellCompiler_LeetCode1(t *testing.T) {
	if _, err := exec.LookPath("runhaskell"); err != nil {
		t.Skip("runhaskell not installed")
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
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("runhaskell error: %v\n%s", err, out)
	}
	expected := "0\n1\n"
	if string(out) != expected {
		t.Fatalf("unexpected output: %q", out)
	}
}
