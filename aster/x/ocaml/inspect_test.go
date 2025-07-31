//go:build slow

package ocaml_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	ocaml "mochi/aster/x/ocaml"
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

func ensureNode(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not installed")
	}
}

func TestInspect_Golden(t *testing.T) {
	ensureNode(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	outDir := filepath.Join(root, "tests", "aster", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.ml"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 120 {
		files = files[:120]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".ml")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := ocaml.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".ocaml.json")
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
