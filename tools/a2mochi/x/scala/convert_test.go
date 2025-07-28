//go:build slow

package scala_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	scala "mochi/tools/a2mochi/x/scala"
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
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/scala", "*.scala")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/scala")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".scala")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			file, err := scala.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			src, err := scala.ConvertSource(file)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			astNode, err := scala.Convert(file)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0644)
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
