//go:build slow

package rs_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	rs "mochi/aster/x/rs"
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

func TestInspect_Golden(t *testing.T) {
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
	outDir := filepath.Join(root, "tests", "aster", "x", "rs")
	os.MkdirAll(outDir, 0o755)

       files, err := filepath.Glob(filepath.Join(srcDir, "*.rs"))
       if err != nil {
               t.Fatal(err)
       }
       sort.Strings(files)
       var selected []string
       for _, f := range files {
               name := strings.TrimSuffix(filepath.Base(f), ".rs")
               if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
                       continue
               }
               selected = append(selected, f)
               if len(selected) >= 10 {
                       break
               }
       }
       files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".rs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := rs.Inspect(string(data), rs.Option{Positions: true})
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".rs.json")
			if shouldUpdate() {
				if err := os.WriteFile(goldenPath, out, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			}
			want, err := os.ReadFile(goldenPath)
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
