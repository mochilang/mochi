//go:build slow

package cppcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cppcode "mochi/compile/cpp"
	"mochi/parser"
	"mochi/types"
)

// TestCPPCompiler_TwoSum compiles the LeetCode example to C++ and runs it.
func TestCPPCompiler_TwoSum(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
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
	c := cppcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "prog.cpp")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	bin := filepath.Join(dir, "prog")
	if out, err := exec.Command(cpp, file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("cpp error: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := string(bytes.TrimSpace(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}
