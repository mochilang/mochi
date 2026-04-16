//go:build slow
// +build slow

package spoj_test

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestSpojPrograms(t *testing.T) {
	rootDir := findRoot(t)
	base := filepath.Join(rootDir, "tests/spoj/x/mochi")
	var mochiFiles []string
	err := filepath.WalkDir(base, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(path, ".mochi") {
			mochiFiles = append(mochiFiles, path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk error: %v", err)
	}
	if len(mochiFiles) == 0 {
		t.Fatalf("no .mochi files found under %s", base)
	}

	for _, src := range mochiFiles {
		src := src
		rel, _ := filepath.Rel(rootDir, src)
		name := strings.TrimSuffix(rel, ".mochi")
		t.Run(strings.ReplaceAll(name, string(filepath.Separator), "/"), func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("typecheck: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile: %v", err)
			}
			in := bytes.NewReader([]byte{})
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				in = bytes.NewReader(data)
			}
			var out bytes.Buffer
			m := vm.NewWithIO(p, in, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run: %v", err)
			}
			got := bytes.TrimSpace(out.Bytes())
			outPath := strings.TrimSuffix(src, ".mochi") + ".out"
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing .out: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, want)
			}
		})
	}
}

func findRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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
