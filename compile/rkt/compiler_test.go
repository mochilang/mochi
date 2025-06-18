//go:build slow

package rktcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rktcode "mochi/compile/rkt"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
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
	c := rktcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rkt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("racket", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("racket run error: %v\n%s", err, out)
	}
	res := bytes.TrimSpace(out)
	if string(res) != "0\n1" {
		t.Fatalf("unexpected output: %q", res)
	}
}
