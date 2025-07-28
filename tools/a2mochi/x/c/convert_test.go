//go:build slow

package c_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	c "mochi/tools/a2mochi/x/c"
)

var update = flag.Bool("update", false, "update golden files")

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

func TestConvert_Golden(t *testing.T) {
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skipf("clang not installed: %v", err)
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/c", "*.c")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	skip := map[string]bool{
		"bench_block":              true,
		"break_continue":           true,
		"closure":                  true,
		"cross_join":               true,
		"cross_join_filter":        true,
		"cross_join_triple":        true,
		"dataset_sort_take_limit":  true,
		"dataset_where_filter":     true,
		"for_list_collection":      true,
		"for_map_collection":       true,
		"group_by":                 true,
		"group_by_join":            true,
		"group_by_left_join":       true,
		"group_by_multi_join":      true,
		"group_by_multi_join_sort": true,
		"if_then_else":             true,
		"if_then_else_nested":      true,
		"in_operator_extended":     true,
		"inner_join":               true,
		"join_multi":               true,
		"left_join":                true,
		"left_join_multi":          true,
		"match_expr":               true,
		"match_full":               true,
		"membership":               true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/c")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".c")
		if skip[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			src, err := c.ConvertSource(string(data))
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			astNode, err := c.Convert(string(data))
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, got, 0644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
