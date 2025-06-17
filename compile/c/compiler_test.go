package ccode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/compile/c"
	"mochi/parser"
	"mochi/types"
)

// TestCCompiler_TwoSum compiles the LeetCode example to C and runs it.
func TestCCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("gcc"); err != nil {
		t.Skip("gcc not installed")
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
	c := ccode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	goldenPath := filepath.Join("..", "..", "tests", "compiler", "valid", "two_sum.c.out")
	golden, err := os.ReadFile(goldenPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	gotSrc := strings.TrimSpace(string(code))
	wantSrc := strings.TrimSpace(string(golden))
	if gotSrc != wantSrc {
		t.Fatalf("generated C mismatch\n--- got ---\n%s\n--- want ---\n%s", gotSrc, wantSrc)
	}
	dir := t.TempDir()
	cfile := filepath.Join(dir, "prog.c")
	if err := os.WriteFile(cfile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	bin := filepath.Join(dir, "prog")
	if out, err := exec.Command("gcc", cfile, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("gcc error: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}
