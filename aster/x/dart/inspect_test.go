//go:build slow

package dart_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	dart "mochi/aster/x/dart"
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
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
	outDir := filepath.Join(root, "tests", "aster", "x", "dart")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.dart"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	sel := map[string]bool{
		"append_builtin.dart":          true,
		"avg_builtin.dart":             true,
		"basic_compare.dart":           true,
		"bench_block.dart":             true,
		"binary_precedence.dart":       true,
		"bool_chain.dart":              true,
		"break_continue.dart":          true,
		"cast_string_to_int.dart":      true,
		"cast_struct.dart":             true,
		"closure.dart":                 true,
		"count_builtin.dart":           true,
		"cross_join.dart":              true,
		"cross_join_filter.dart":       true,
		"cross_join_triple.dart":       true,
		"dataset_sort_take_limit.dart": true,
		"dataset_where_filter.dart":    true,
		"exists_builtin.dart":          true,
		"for_list_collection.dart":     true,
		"for_loop.dart":                true,
		"for_map_collection.dart":      true,
		"fun_call.dart":                true,
		"fun_expr_in_let.dart":         true,
		"fun_three_args.dart":          true,
		"go_auto.dart":                 true,
		"group_by.dart":                true,
	}
	var selected []string
	for _, f := range files {
		if sel[filepath.Base(f)] {
			selected = append(selected, f)
		}
	}
	files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".dart")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := dart.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			out, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out = append(out, '\n')
			goldenPath := filepath.Join(outDir, name+".dart.json")
			if *update {
				if err := os.WriteFile(goldenPath, out, 0o644); err != nil {
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
