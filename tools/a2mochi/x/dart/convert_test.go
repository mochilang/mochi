//go:build slow

package dart_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/dart"
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

func runMochi(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, err
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestConvert_Golden(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skipf("go not installed: %v", err)
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/dart", "*.dart")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"append_builtin":           true,
		"avg_builtin":              true,
		"basic_compare":            true,
		"bench_block":              true,
		"binary_precedence":        true,
		"bool_chain":               true,
		"break_continue":           true,
		"cast_string_to_int":       true,
		"cast_struct":              true,
		"closure":                  true,
		"count_builtin":            true,
		"cross_join":               true,
		"cross_join_filter":        true,
		"cross_join_triple":        true,
		"dataset_sort_take_limit":  true,
		"dataset_where_filter":     true,
		"exists_builtin":           true,
		"for_list_collection":      true,
		"for_loop":                 true,
		"for_map_collection":       true,
		"fun_call":                 true,
		"fun_expr_in_let":          true,
		"fun_three_args":           true,
		"go_auto":                  true,
		"group_by":                 true,
		"group_by_conditional_sum": true,
		"group_by_having":          true,
		"group_by_join":            true,
		"group_by_left_join":       true,
		"group_by_multi_join":      true,
		"group_by_multi_join_sort": true,
		"group_by_multi_sort":      true,
		"group_by_sort":            true,
		"group_items_iteration":    true,
		"if_else":                  true,
		"if_then_else":             true,
		"if_then_else_nested":      true,
		"in_operator":              true,
		"in_operator_extended":     true,
		"inner_join":               true,
		"join_multi":               true,
		"json_builtin":             true,
		"left_join":                true,
		"left_join_multi":          true,
		"len_builtin":              true,
		"len_map":                  true,
		"len_string":               true,
		"let_and_print":            true,
		"list_assign":              true,
		"list_index":               true,
		"list_nested_assign":       true,
		"list_set_ops":             true,
		"load_yaml":                true,
		"map_assign":               true,
		"map_in_operator":          true,
		"map_index":                true,
		"map_int_key":              true,
		"map_literal_dynamic":      true,
		"map_membership":           true,
		"map_nested_assign":        true,
		"match_expr":               true,
		"match_full":               true,
		"math_ops":                 true,
		"membership":               true,
		"min_max_builtin":          true,
		"nested_function":          true,
		"order_by_map":             true,
		"outer_join":               true,
		"partial_application":      true,
		"print_hello":              true,
		"pure_fold":                true,
		"pure_global_fold":         true,
		"python_auto":              true,
		"python_math":              true,
		"query_sum_select":         true,
		"record_assign":            true,
		"right_join":               true,
		"save_jsonl_stdout":        true,
		"short_circuit":            true,
		"slice":                    true,
		"sort_stable":              true,
		"str_builtin":              true,
		"string_compare":           true,
		"string_concat":            true,
		"string_contains":          true,
		"string_in_operator":       true,
		"string_index":             true,
		"string_prefix_slice":      true,
		"substring_builtin":        true,
		"sum_builtin":              true,
		"tail_recursion":           true,
		"test_block":               true,
		"tree_sum":                 true,
		"two-sum":                  true,
		"typed_let":                true,
		"typed_var":                true,
		"unary_neg":                true,
		"update_stmt":              true,
		"user_type_literal":        true,
                "values_builtin":           true,
                "var_assignment":           true,
                "while_loop":               true,
                "simple_loop":              true,
        }

	outDir := filepath.Join(root, "tests/a2mochi/x/dart")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".dart")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := dart.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			src, err := dart.ConvertSource(node)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			astNode, err := dart.Convert(node)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0644)
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			var buf bytes.Buffer
			if err := ast.Fprint(&buf, astNode); err != nil {
				t.Fatalf("print: %v", err)
			}
			code := buf.String()
                        mochiPath := filepath.Join(outDir, name+".mochi")
                        if *update {
                                os.WriteFile(mochiPath, []byte(code), 0644)
                        }
                        gotOut, err := runMochi(code)
                        if err != nil {
                                t.Fatalf("run: %v", err)
                        }
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
                        wantOut, err := runMochi(string(vmSrc))
                        if err != nil {
                                t.Fatalf("run vm: %v", err)
                        }
                        if *update {
                                os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0644)
                        }
                        if !bytes.Equal(gotOut, wantOut) {
                                t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
                        }
		})
	}
}
