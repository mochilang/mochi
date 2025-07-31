//go:build slow

package c_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	c "mochi/aster/x/c"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	t.Helper()
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
	t.Fatal("go.mod not found")
	return ""
}

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestInspectGolden(t *testing.T) {
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "c")
	outDir := filepath.Join(root, "tests", "aster", "x", "c")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.c"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

       if len(files) > 25 {
               files = files[:25]
       }

	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".c")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			prog, err := c.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			b, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			golden := filepath.Join(outDir, name+".c.json")
			if shouldUpdate() {
				if err := os.WriteFile(golden, append(b, '\n'), 0o644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			}
			want, err := os.ReadFile(golden)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(bytes.TrimSpace(b), want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", b, want)
			}
		})
	}
}
