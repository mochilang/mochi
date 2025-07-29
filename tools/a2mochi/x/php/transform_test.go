//go:build slow

package php_test

import (
	"bytes"
	"flag"
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

func runMochi(t *testing.T, src string) []byte {
	t.Helper()
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatal(errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		t.Fatal(err)
	}
	return bytes.TrimSpace(out.Bytes())
}

func parseProgram(t *testing.T, path string) *php.Program {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	prog, err := php.Parse(string(data))
	if err != nil {
		t.Fatalf("parse %s: %v", path, err)
	}
	return prog
}

func transformProg(t *testing.T, p *php.Program) *ast.Node {
	t.Helper()
	node, err := php.Transform(p)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	return node
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
		"bool_chain":          true,
		"len_map":             true,
		"len_string":          true,
		"substring_builtin":   true,
		"string_contains":     true,
		"string_in_operator":  true,
		"string_index":        true,
		"string_prefix_slice": true,
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
		"membership":          true,
		"map_membership":      true,
		"in_operator":         true,
		"map_in_operator":     true,
		"for_map_collection":  true,
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "php")
	os.MkdirAll(outDir, 0o755)
	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".php")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			prog := parseProgram(t, path)
			node := transformProg(t, prog)
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
			var buf bytes.Buffer
			if err := ast.Fprint(&buf, node); err != nil {
				t.Fatalf("print: %v", err)
			}
			code := buf.String()
			gotOut := runMochi(t, code)
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut := runMochi(t, string(vmSrc))
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}
