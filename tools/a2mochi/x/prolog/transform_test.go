//go:build slow

package prolog_test

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
	"mochi/types"

	prolog "mochi/tools/a2mochi/x/prolog"
)

var update = flag.Bool("update", false, "update golden files")

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
	mach := vm.New(p, &out)
	if err := mach.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

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

func transformFromFile(t *testing.T, path string) (string, *ast.Node) {
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := prolog.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := prolog.TransformProgram(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	code, err := prolog.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	return code, node
}

func TestTransform(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/pl", "*.pl")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowedSet := map[string]bool{
		"avg_builtin":         true,
		"print_hello":         true,
		"unary_neg":           true,
		"for_loop":            true,
		"len_builtin":         true,
		"sum_builtin":         true,
		"let_and_print":       true,
		"basic_compare":       true,
		"if_else":             true,
		"while_loop":          true,
		"append_builtin":      true,
		"cast_string_to_int":  true,
		"len_string":          true,
		"string_concat":       true,
		"string_index":        true,
		"string_compare":      true,
		"list_index":          true,
		"string_contains":     true,
		"bool_chain":          true,
		"str_builtin":         true,
		"var_assignment":      true,
		"len_map":             true,
		"list_assign":         true,
		"map_assign":          true,
		"list_nested_assign":  true,
		"count_builtin":       true,
		"cast_struct":         true,
		"map_nested_assign":   true,
		"map_int_key":         true,
		"map_index":           true,
		"map_literal_dynamic": true,
		"membership":          true,
		"in_operator":         true,
		"string_in_operator":  true,
		"substring_builtin":   true,
		"values_builtin":      true,
		"for_list_collection": true,
		"for_map_collection":  true,
		"map_in_operator":     true,
		"map_membership":      true,
		"min_max_builtin":     true,
		"if_then_else_nested": true,
		"string_prefix_slice": true,
		"list_set_ops":        true,
		"json_builtin":        true,
		"test_block":          true,
	}

	outputDir := filepath.Join(root, "tests/a2mochi/x/prolog")
	os.MkdirAll(outputDir, 0o755)

	for _, srcFile := range files {
		name := strings.TrimSuffix(filepath.Base(srcFile), ".pl")
		if !allowedSet[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			code, node := transformFromFile(t, srcFile)

			astPath := filepath.Join(outputDir, name+".ast")
			errPath := filepath.Join(outputDir, name+".error")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				os.WriteFile(filepath.Join(outputDir, name+".mochi"), []byte(code), 0o644)
				os.Remove(errPath)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := []byte(node.String())
			if string(want) != string(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			gotOut, err := runMochi(code)
			if err != nil {
				if *update {
					os.WriteFile(errPath, []byte(err.Error()), 0o644)
				}
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outputDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := runMochi(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				if *update {
					msg := fmt.Sprintf("output mismatch\n-- got --\n%s\n-- want --\n%s", gotOut, wantOut)
					os.WriteFile(errPath, []byte(msg), 0o644)
				}
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			} else if *update {
				os.Remove(errPath)
			}
		})
	}
}

func updateReadme() {
	prolog.UpdateReadmeForTests()
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
