//go:build slow

package plcode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	plcode "mochi/compile/pl"
	"mochi/parser"
	"mochi/types"
)

func TestPrologCompiler_LeetCode1(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
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
	c := plcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.pl")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("swipl", "-q", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("swipl error: %v\n%s", err, out)
	}
	expected := "0\n1"
	if strings.TrimSpace(string(out)) != expected {
		t.Fatalf("unexpected output: %q", out)
	}
}
