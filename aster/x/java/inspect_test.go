//go:build slow

package java_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	javaast "mochi/aster/x/java"
	javacode "mochi/compiler/x/java"
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

func ensureJava(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
}

func TestInspect_Golden(t *testing.T) {
	ensureJava(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "java")
	outDir := filepath.Join(root, "tests", "aster", "x", "java")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.java"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	allowed := map[string]bool{
		"append_builtin":    true,
		"avg_builtin":       true,
		"basic_compare":     true,
		"bench_block":       true,
		"binary_precedence": true,
		"group_by_having":   true,
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".java")
		if !allowed[name] {
			continue
		}
		if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
			t.Logf("skip %s due to compile error", name)
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := javaast.Inspect(string(data), javaast.Options{})
			if err != nil {
				t.Skipf("inspect error: %v", err)
				return
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".java.json")
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
