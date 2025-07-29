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
	allowed := map[string]bool{
		"append_builtin":      true,
		"basic_compare":       true,
		"binary_precedence":   true,
		"bool_chain":          true,
		"break_continue":      true,
		"cast_string_to_int":  true,
		"print_hello":         true,
		"avg_builtin":         true,
		"sum_builtin":         true,
		"closure":             true,
		"fun_call":            true,
		"for_loop":            true,
		"len_builtin":         true,
		"len_string":          true,
		"map_index":           true,
		"if_else":             true,
		"while_loop":          true,
		"unary_neg":           true,
		"let_and_print":       true,
		"list_index":          true,
		"fun_three_args":      true,
		"fun_expr_in_let":     true,
		"string_concat":       true,
		"string_compare":      true,
		"string_index":        true,
		"string_prefix_slice": true,
		"slice":               true,
		"list_assign":         true,
		"map_assign":          true,
		"membership":          true,
		"map_membership":      true,
		"count_builtin":       true,
		"math_ops":            true,
		"min_max_builtin":     true,
		"if_then_else":        true,
		"len_map":             true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"exists_builtin":      true,
		"list_set_ops":        true,
		"map_in_operator":     true,
		"map_nested_assign":   true,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "py")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".py")
		if !allowed[name] {
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
			var buf bytes.Buffer
			if err := ast.Fprint(&buf, node); err != nil {
				t.Fatalf("print: %v", err)
			}
			t.Log(buf.String())

			code, err := py.ConvertSource(n)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
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
