//go:build slow

package kotlin_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	kotlin "mochi/aster/x/kotlin"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
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

func TestInspect_Golden(t *testing.T) {
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	outDir := filepath.Join(root, "tests", "aster", "x", "kotlin")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.kt"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".kt")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := kotlin.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			golden := filepath.Join(outDir, name+".kt.json")
			if *update {
				if err := os.WriteFile(golden, out, 0o644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				return
			}
			want, err := os.ReadFile(golden)
			if err != nil {
				t.Skip("missing golden")
				return
			}
			if string(out) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", out, want)
			}
		})
	}
}
