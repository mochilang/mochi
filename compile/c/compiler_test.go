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

// Test that the C compiler can compile and run the first LeetCode example.
func TestCCompiler_LeetCode1(t *testing.T) {
	if _, err := exec.LookPath("cython"); err != nil {
		t.Skip("cython not installed")
	}
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
	comp := ccode.New(env)
	code, err := comp.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	dir := t.TempDir()
	cfile := filepath.Join(dir, "main.c")
	if err := os.WriteFile(cfile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "prog")

	incOut, err := exec.Command("python3-config", "--includes").Output()
	if err != nil {
		t.Fatalf("python3-config error: %v", err)
	}
	ldOut, err := exec.Command("python3-config", "--ldflags").Output()
	if err != nil {
		t.Fatalf("python3-config error: %v", err)
	}

	args := append([]string{"-Os"}, strings.Fields(string(incOut))...)
	args = append(args, cfile)
	args = append(args, strings.Fields(string(ldOut))...)
	args = append(args, "-lpython3.11", "-o", exe)

	cmd := exec.Command("gcc", args...)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("gcc error: %v\n%s", err, out)
	}

	run := exec.Command(exe)
	out, err := run.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}

	got := strings.TrimSpace(string(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output:\n%s", out)
	}
}
