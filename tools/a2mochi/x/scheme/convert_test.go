//go:build slow

package scheme_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/a2mochi/x/scheme"
)

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

func TestParseConvert(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "scheme", "*.scm")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".scm")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			srcStr := string(data)
			items, err := scheme.Parse(srcStr)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			if _, err := scheme.Convert(items, srcStr); err != nil {
				t.Fatalf("convert: %v", err)
			}
		})
	}
}
