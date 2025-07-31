//go:build slow

package cpp_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"mochi/aster/x/cpp"
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
	pattern := filepath.Join(root, "tests", "transpiler", "x", "cpp", "*.cpp")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var selected []string
	idx := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".cpp")
		if _, err := os.Stat(filepath.Join(filepath.Dir(f), name+".error")); err == nil {
			continue
		}
		if name == "bench_block" {
			continue
		}
		idx++
		if idx > 25 {
			break
		}
		selected = append(selected, f)
	}
	files = selected

	outDir := filepath.Join(root, "tests", "aster", "x", "cpp")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".cpp")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := cpp.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			got, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			golden := filepath.Join(outDir, name+".cpp.json")
			if *update {
				os.WriteFile(golden, got, 0o644)
			}
			want, err := os.ReadFile(golden)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(want) != string(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
