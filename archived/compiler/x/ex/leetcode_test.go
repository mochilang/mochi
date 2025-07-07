//go:build archived && slow

package excode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	excode "mochi/archived/x/ex"
	"mochi/parser"
	"mochi/types"
)

// compileAndRunLeetCode compiles the Mochi solution for the given problem ID and
// executes the generated Elixir code, returning stdout.
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
	c := excode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "main.exs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("elixir", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("elixir run error: %v\n%s", err, out)
	}
	return strings.ReplaceAll(string(out), "\r\n", "\n")
}

func TestLeetCode1(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "1"))
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode2(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "2"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode3(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "3"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode4(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "4"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode5(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "5"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode6(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "6"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode7(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "7"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode8(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "8"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode9(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "9"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLeetCode10(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	got := strings.TrimSpace(compileAndRunLeetCode(t, "10"))
	if got != "" {
		t.Fatalf("unexpected output: %q", got)
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
