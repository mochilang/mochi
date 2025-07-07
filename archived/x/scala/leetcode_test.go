//go:build archived && slow

package scalacode_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	scalacode "mochi/archived/x/scala"
	"mochi/parser"
	"mochi/types"
)

// compileAndRunLeetCode compiles the Mochi solution for the given problem ID and
// executes the generated Scala code, returning stdout.
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
	c := scalacode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "Main.scala")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	scalacCmd := exec.Command("scalac", filepath.Base(file))
	scalacCmd.Dir = tmp
	out, err := scalacCmd.CombinedOutput()
	if err != nil {
		t.Fatalf("scalac error: %v\n%s", err, out)
	}
	cmd := exec.Command("scala", "Main")
	cmd.Dir = tmp
	out, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("scala run error: %v\n%s", err, out)
	}
	return strings.ReplaceAll(string(out), "\r\n", "\n")
}

func TestLeetCodeSuite(t *testing.T) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	for i := 1; i <= 9; i++ {
		id := fmt.Sprint(i)
		t.Run(id, func(t *testing.T) {
			got := strings.TrimSpace(compileAndRunLeetCode(t, id))
			if i == 1 {
				if got != "0\n1" {
					t.Fatalf("unexpected output: %q", got)
				}
			} else if got != "" {
				t.Fatalf("unexpected output: %q", got)
			}
		})
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
