//go:build slow

package mochi_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	mochi "mochi/aster/x/mochi"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
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

func TestInspect_Golden(t *testing.T) {
	root := repoRoot(t)
	srcPattern := filepath.Join(root, "tests", "transpiler", "x", "mochi", "*.mochi")
	files, err := filepath.Glob(srcPattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", srcPattern)
	}

	outDir := filepath.Join(root, "tests", "aster", "x", "mochi")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := mochi.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			js, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			outPath := filepath.Join(outDir, name+".mochi.json")
			if *update {
				if err := os.WriteFile(outPath, append(js, '\n'), 0o644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				return
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if strings.TrimSpace(string(js)) != strings.TrimSpace(string(want)) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", js, want)
			}
		})
	}
}
