//go:build slow

package dart_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	dart "mochi/aster/x/dart"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func ensureDart(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureDart(t)
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
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".dart.json")
			if shouldUpdate() {
				if err := os.WriteFile(jsonPath, astJSON, 0644); err != nil {
					t.Fatalf("write json: %v", err)
				}
			}
			wantJSON, err := os.ReadFile(jsonPath)
			if err != nil {
				t.Skip("missing golden")
				return
			}
			if string(astJSON) != string(wantJSON) {
				t.Fatalf("json mismatch\n--- got ---\n%s\n--- want ---\n%s", astJSON, wantJSON)
			}
			out, err := dart.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".dart")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			cmd := exec.Command("dart", outPath)
			got, _ := cmd.CombinedOutput()
			want, _ := exec.Command("dart", src).CombinedOutput()
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, got, 0644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if string(got) != string(want) {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
			}
		})
	}
}
