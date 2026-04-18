//go:build slow

package spoj

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestMochiSolutions(t *testing.T) {
	rootDir := findTestRoot(t)
	base := filepath.Join(rootDir, "tests/spoj/x/mochi")
	// Walk all subdirectories (00000, 00100, ...) for .mochi files.
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
			input, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
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
			var out bytes.Buffer
			inPath := strings.TrimSuffix(src, ".mochi") + ".in"
			if data, err := os.ReadFile(inPath); err == nil {
				m := vm.NewWithIO(p, bytes.NewReader(data), &out)
				if err := m.Run(); err != nil {
					t.Fatalf("run: %v", err)
				}
			} else {
				m := vm.New(p, &out)
				if err := m.Run(); err != nil {
					t.Fatalf("run: %v", err)
				}
			}
			got := bytes.TrimSpace(out.Bytes())
			outPath := strings.TrimSuffix(src, ".mochi") + ".out"
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing .out file: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n\n--- Source ---\n%s", got, want, input)
			}
		})
	}
}

func findTestRoot(t *testing.T) string {
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

// goldenRun is kept for other language tests.
var _ = golden.Run
