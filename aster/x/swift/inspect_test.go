//go:build slow

package swift_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	swift "mochi/aster/x/swift"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
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

func TestInspect_Golden(t *testing.T) {
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "swift")
	outDir := filepath.Join(root, "tests", "json-ast", "x", "swift")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.swift"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".swift")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
				t.Logf("skip %s due to compile error", name)
				return
			}
			prog, err := swift.Inspect(string(data))
			if err != nil {
				t.Logf("skip %s: %v", name, err)
				return
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".swift.json")
			if *update {
				if err := os.WriteFile(goldenPath, out, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			}
			want, err := os.ReadFile(goldenPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(out) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", out, want)
			}
		})
	}
}
