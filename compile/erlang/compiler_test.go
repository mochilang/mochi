package erlcode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	erlcode "mochi/compile/erlang"
	"mochi/parser"
	"mochi/types"
)

func TestErlangCompiler_LeetCode1(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
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
	c := erlcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.erl")
	if err := os.WriteFile(file, code, 0755); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("escript", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("escript error: %v\n%s", err, out)
	}
	expected := "0\n1\n"
	if string(out) != expected {
		t.Fatalf("unexpected output: %q", out)
	}
}
