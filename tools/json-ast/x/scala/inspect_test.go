//go:build slow

package scala_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	scala "mochi/tools/json-ast/x/scala"
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

func ensureScala(t *testing.T) {
	if _, err := exec.LookPath("scala"); err != nil {
		t.Skip("scala not installed")
	}
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
}

func TestInspect_Golden(t *testing.T) {
	ensureScala(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "scala")
	outDir := filepath.Join(root, "tests", "json-ast", "x", "scala")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.scala"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".scala")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
               prog, err := scala.Inspect(string(data))
               if err != nil {
                       t.Skipf("inspect: %v", err)
               }
               out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".scala.json")
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
