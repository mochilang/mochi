//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestMochiTasks(t *testing.T) {
	root := findRepoRoot(t)
	base := filepath.Join(root, "tests/rosetta/x/Mochi")

	outs, err := filepath.Glob(filepath.Join(base, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, out := range outs {
		name := strings.TrimSuffix(filepath.Base(out), ".out")
		src := filepath.Join(base, name+".mochi")
		if _, err := os.Stat(src); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			got, err := runMochi(src)
			if err != nil {
				t.Fatalf("run error: %v", err)
			}
			want, err := os.ReadFile(out)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}

			got = bytes.TrimSpace(got)
			want = bytes.TrimSpace(want)

			if !bytes.Equal(got, want) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
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
