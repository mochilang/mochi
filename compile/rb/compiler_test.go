package rbcode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rbcode "mochi/compile/rb"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
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
