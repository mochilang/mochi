//go:build slow

package php_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/tools/a2mochi/x/php"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	t.Helper()
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

func parseProgram(path string) (*php.Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	prog, err := php.Parse(string(data))
	if err != nil {
		return nil, fmt.Errorf("parse %s: %w", path, err)
	}
	return prog, nil
}

func transformProg(p *php.Program) (*ast.Node, error) {
	node, err := php.Transform(p)
	if err != nil {
		return nil, err
	}
	return node, nil
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "php", "*.php")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"unary_neg":           true,
		"print_hello":         true,
		"let_and_print":       true,
		"list_index":          true,
		"avg_builtin":         true,
		"sum_builtin":         true,
		"len_builtin":         true,
		"count_builtin":       true,
		"basic_compare":       true,
		"string_concat":       true,
		"string_compare":      true,
		"binary_precedence":   true,
		"cast_string_to_int":  true,
		"cast_struct":         true,
		"bool_chain":          true,
		"len_map":             true,
		"len_string":          true,
		"substring_builtin":   true,
		"string_contains":     true,
		"string_in_operator":  true,
		"string_index":        true,
		"string_prefix_slice": true,
		"slice":               true,
		"var_assignment":      true,
		"user_type_literal":   true,
		"if_else":             true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"for_loop":            true,
		"for_list_collection": true,
		"while_loop":          true,
		"append_builtin":      true,
		"break_continue":      true,
		"list_assign":         true,
		"map_assign":          true,
		"list_nested_assign":  true,
		"map_nested_assign":   true,
		"map_index":           true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"bigint_ops":          true,
		"math_ops":            true,
		"min_max_builtin":     true,
		"values_builtin":      true,
		"membership":          true,
		"map_membership":      true,
		"in_operator":         true,
		"map_in_operator":     true,
		"for_map_collection":  true,
		"fun_call":            true,
		"fun_three_args":      true,
		"fun_expr_in_let":     true,
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "php")
	os.MkdirAll(outDir, 0o755)
	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".php")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			if *update {
				os.Remove(filepath.Join(outDir, name+".error"))
			}

			prog, err := parseProgram(path)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("%v", err)
				return
			}
			node, err := transformProg(prog)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("transform: %v", err)
				return
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				code, err := php.ConvertFile(path)
				if err == nil {
					os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(code), 0o644)
				}
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := node.String()
			if strings.TrimSpace(string(want)) != strings.TrimSpace(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
			code, err := php.Print(node)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("print: %v", err)
				return
			}
			gotOut, err := runMochi(code)
			if err != nil {
				if *update {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("run: %v", err)
				return
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				vmSrc, err = os.ReadFile(filepath.Join(root, "tests", "vm_extended", "valid", name+".mochi"))
				if err != nil {
					t.Fatalf("missing vm source: %v", err)
				}
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
	if *update {
		updateReadme()
	}
}

func updateReadme() {
	php.UpdateReadmeForTests()
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
