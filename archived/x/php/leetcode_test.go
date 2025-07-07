//go:build archived && slow

package phpcode_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	phpcode "mochi/archived/x/php"
	"mochi/parser"
	"mochi/types"
)

func compileAndRunLeetCode(t *testing.T, id string) string {
	t.Helper()
	root := findRoot(t)
	dir := filepath.Join(root, "examples", "leetcode", id)
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read dir: %v", err)
	}
	var src string
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
			src = filepath.Join(dir, e.Name())
			break
		}
	}
	if src == "" {
		t.Fatalf("no mochi source for id %s", id)
	}
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
	tmp := t.TempDir()
	file := filepath.Join(tmp, "main.php")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("php", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("php run error: %v\n%s", err, out)
	}
	return strings.ReplaceAll(string(out), "\r\n", "\n")
}

func TestLeetCode1(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "1"))
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCodeProblems(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	for i := 2; i <= 30; i++ {
		id := fmt.Sprint(i)
		got := strings.TrimSpace(compileAndRunLeetCode(t, id))
		if got != "" {
			t.Fatalf("unexpected output for %s: %q", id, got)
		}
	}
}

func findRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}
