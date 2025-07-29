//go:build slow

package gox_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"testing"

	gox "mochi/tools/json-ast/x/go"
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
	srcPattern := filepath.Join(root, "tests/transpiler/x/go", "*.go")
	files, err := filepath.Glob(srcPattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", srcPattern)
	}

	outDir := filepath.Join(root, "tests/json-ast/x/go")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	for _, src := range files {
		name := filepath.Base(src) // includes .go
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := gox.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			js, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			outPath := filepath.Join(outDir, name+".json")
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
			if string(js) != string(bytes.TrimSpace(want)) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", js, want)
			}
		})
	}
}
