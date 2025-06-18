package pascode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pascode "mochi/compile/pas"
	"mochi/parser"
	"mochi/types"
)

// TestPascalCompiler_TwoSum compiles the LeetCode example to Pascal and runs it.
func TestPascalCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skipf("fpc not installed: %v", err)
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
	c := pascode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "prog.pas")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	if out, err := exec.Command("fpc", file).CombinedOutput(); err != nil {
		t.Fatalf("fpc error: %v\n%s", err, out)
	}
	exe := filepath.Join(dir, "prog")
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := string(bytes.TrimSpace(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}
