package ts_test

import (
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	ts "mochi/tools/a2mochi/x/ts"
)

var update = flag.Bool("update", false, "update golden files")

func findRepoRoot(t *testing.T) string {
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

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/ts", "*.ts")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	outDir := filepath.Join(root, "tests/a2mochi/x/ts")
	_ = os.MkdirAll(outDir, 0755)
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".ts")
		t.Run(name, func(t *testing.T) {
			nodes, err := ts.ParseFile(f)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := ts.Convert(nodes)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0644)
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
