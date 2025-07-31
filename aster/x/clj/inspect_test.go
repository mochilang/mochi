//go:build slow

package clj_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	clj "mochi/aster/x/clj"
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

func ensureBabashka(t *testing.T) {
	if err := clj.EnsureBabashka(); err != nil {
		t.Skipf("bb not installed: %v", err)
	}
}

func TestInspect_Golden(t *testing.T) {
	ensureBabashka(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	outDir := filepath.Join(root, "tests", "aster", "x", "clj")
	os.MkdirAll(outDir, 0o755)

	files := []string{
		"append_builtin.clj",
		"avg_builtin.clj",
		"basic_compare.clj",
		"bench_block.clj",
		"binary_precedence.clj",
	}
	for i, f := range files {
		files[i] = filepath.Join(srcDir, f)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".clj")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := clj.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".clj.json")
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
