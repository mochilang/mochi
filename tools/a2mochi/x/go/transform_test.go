//go:build slow

package gox_test

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	gox "mochi/tools/a2mochi/x/go"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
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

func mochiOutput(src string) ([]byte, error) {
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

func testFile(t *testing.T, root, outDir, srcPath string) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".go")
	src, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := gox.Parse(string(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	n, err := gox.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	got := []byte(n.String())
	astPath := filepath.Join(outDir, name+".ast")
	if *update {
		os.WriteFile(astPath, got, 0o644)
	}
	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Skipf("missing golden: %v", err)
	}
	if string(got) != string(want) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}

	code, err := gox.Print(n)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *update {
		os.WriteFile(mochiPath, []byte(code), 0o644)
	}
	gotOut, err := mochiOutput(code)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if *update {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
	}
	vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut, err := mochiOutput(string(vmSrc))
	if err != nil {
		t.Fatalf("run vm: %v", err)
	}
	if !bytes.Equal(gotOut, wantOut) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/go", "*.go")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"print_hello":         true,
		"let_and_print":       true,
		"if_else":             true,
		"var_assignment":      true,
		"len_string":          true,
		"string_concat":       true,
		"str_builtin":         true,
		"unary_neg":           true,
		"for_loop":            true,
		"while_loop":          true,
		"basic_compare":       true,
		"binary_precedence":   true,
		"list_index":          true,
		"list_assign":         true,
		"map_index":           true,
		"typed_var":           true,
		"typed_let":           true,
		"len_builtin":         true,
		"len_map":             true,
		"bool_chain":          true,
		"short_circuit":       true,
		"break_continue":      true,
		"map_assign":          true,
		"map_nested_assign":   true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"list_nested_assign":  true,
		"string_compare":      true,
		"string_contains":     true,
		"string_index":        true,
		"string_prefix_slice": true,
		"string_in_operator":  true,
		"substring_builtin":   true,
		"append_builtin":      false,
		"min_max_builtin":     false,
		"slice":               false,
		"sum_builtin":         false,
		"closure":             true,
		"fun_call":            true,
		"fun_expr_in_let":     true,
		"fun_three_args":      true,
		"nested_function":     true,
		"partial_application": true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"match_expr":          true,
		"match_full":          true,
		"cast_string_to_int":  true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/go")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".go")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			testFile(t, root, outDir, srcPath)
		})
	}
}
