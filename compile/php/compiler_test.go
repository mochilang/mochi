package phpcode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	phpcode "mochi/compile/php"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_LeetCodeExample1(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
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
	c := phpcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.php")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("php", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("php run error: %v\n%s", err, out)
	}
	got := strings.ReplaceAll(string(out), "\r\n", "\n")
	if strings.TrimSpace(got) != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}
