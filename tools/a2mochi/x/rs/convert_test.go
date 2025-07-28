package rs_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/ast"

	rs "mochi/tools/a2mochi/x/rs"
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
	if _, err := exec.LookPath("rust-analyzer"); err != nil {
		t.Skipf("rust-analyzer not installed: %v", err)
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/rs", "*.rs")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/rs")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".rs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := rs.Convert(string(data))
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(node.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0644)
				var buf bytes.Buffer
				if err := ast.Fprint(&buf, node); err == nil {
					os.WriteFile(filepath.Join(outDir, name+".mochi"), buf.Bytes(), 0644)
				}
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
