//go:build slow

package zig_test

import (
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/a2mochi/x/zig"
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
	pattern := filepath.Join(root, "tests/transpiler/x/zig", "*.zig")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"print_hello":       true,
		"unary_neg":         true,
		"pure_fold":         true,
		"tail_recursion":    true,
		"binary_precedence": true,
		"avg_builtin":       true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/zig")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".zig")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			mochiSrc, err := zig.ConvertSource(string(data))
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			node, err := zig.Convert(string(data))
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, []byte(node.String()), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(mochiSrc), 0o644)
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := []byte(node.String())
			if string(want) != string(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
