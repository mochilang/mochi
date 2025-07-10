//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestMochiTasks(t *testing.T) {
	root := findRepoRoot(t)
	base := filepath.Join(root, "tests/rosetta/x/Mochi")

	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read dir: %v", err)
	}

	found := false
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		taskDir := filepath.Join(base, e.Name())
		outs, err := filepath.Glob(filepath.Join(taskDir, "*.out"))
		if err != nil {
			t.Fatalf("glob: %v", err)
		}
		if len(outs) == 0 {
			continue
		}
		found = true
		golden.Run(t, filepath.Join("tests/rosetta/x/Mochi", e.Name()), ".mochi", ".out", runMochi)
	}

	if !found {
		t.Fatal("no Mochi Rosetta tests found")
	}
}

func runMochi(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(src, errs[0])
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("run error: %w", err)
	}
	removeErr(src)
	b := bytes.TrimSpace(out.Bytes())
	if b == nil {
		b = []byte{}
	}
	return b, nil
}

func writeErr(src string, err error) {
	errPath := strings.TrimSuffix(src, filepath.Ext(src)) + ".error"
	_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
}

func removeErr(src string) {
	errPath := strings.TrimSuffix(src, filepath.Ext(src)) + ".error"
	os.Remove(errPath)
}

func findRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
