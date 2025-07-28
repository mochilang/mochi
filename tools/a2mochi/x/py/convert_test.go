//go:build slow

package py_test

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

	"mochi/tools/a2mochi/x/py"
)

var update = flag.Bool("update", false, "update golden files")

func findRepoRoot(t *testing.T) string {
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
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "py", "*.py")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "py")
	os.MkdirAll(outDir, 0o755)

	allowed := map[string]bool{
		"append_builtin":      true,
		"avg_builtin":         true,
		"basic_compare":       true,
		"break_continue":      true,
		"cast_string_to_int":  true,
		"cast_struct":         true,
		"count_builtin":       true,
		"exists_builtin":      true,
		"for_list_collection": true,
		"for_loop":            true,
		"for_map_collection":  true,
		"fun_call":            true,
		"fun_expr_in_let":     true,
		"fun_three_args":      true,
		"go_auto":             true,
		"if_else":             true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"len_builtin":         true,
		"len_map":             true,
		"len_string":          true,
		"let_and_print":       true,
		"list_assign":         true,
		"list_index":          true,
		"map_assign":          true,
		"map_in_operator":     true,
		"map_index":           true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"map_membership":      true,
		"match_expr":          true,
		"membership":          true,
		"min_max_builtin":     true,
		"nested_function":     true,
		"print_hello":         true,
		"pure_fold":           true,
		"pure_global_fold":    true,
		"slice":               true,
		"str_builtin":         true,
		"string_compare":      true,
		"string_concat":       true,
		"string_contains":     true,
		"string_in_operator":  true,
		"string_index":        true,
		"string_prefix_slice": true,
		"substring_builtin":   true,
		"sum_builtin":         true,
		"tail_recursion":      true,
		"unary_neg":           true,
		"user_type_literal":   true,
		"var_assignment":      true,
		"while_loop":          true,
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".py")
		if !allowed[name] {
			continue
		}
		astPath := filepath.Join(outDir, name+".ast")
		if _, err := os.Stat(astPath); err != nil {
			if !*update {
				continue
			}
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			n, err := py.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			node, err := py.Convert(n)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := node.String()
			if strings.TrimSpace(string(want)) != strings.TrimSpace(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			var buf bytes.Buffer
			if err := ast.Fprint(&buf, node); err != nil {
				t.Fatalf("print: %v", err)
			}
			code := buf.String()
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := runMochi(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}
