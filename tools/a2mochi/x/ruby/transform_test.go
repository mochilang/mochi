//go:build slow

package ruby_test

import (
	"bytes"
	"encoding/json"
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

	ruby "mochi/tools/a2mochi/x/ruby"
)

var updateGolden = flag.Bool("update", false, "update golden files")

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

func run(src string) ([]byte, error) {
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

func parseFile(t *testing.T, path string) *ruby.Node {
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	node, err := ruby.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return node
}

func transformNode(t *testing.T, n *ruby.Node) *ast.Node {
	node, err := ruby.Transform(n)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	return node
}

func printNode(t *testing.T, n *ast.Node) string {
	src, err := ruby.Print(n)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	return src
}

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/rb", "*.rb")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"break_continue":      true,
		"cast_string_to_int":  true,
		"count_builtin":       true,
		"for_list_collection": true,
		"for_map_collection":  true,
		"if_else":             true,
		"len_builtin":         true,
		"len_map":             true,
		"len_string":          true,
		"let_and_print":       true,
		"list_assign":         true,
		"list_index":          true,
		"map_assign":          true,
		"map_index":           true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"map_nested_assign":   true,
		"membership":          true,
		"print_hello":         true,
		"str_builtin":         true,
		"string_concat":       true,
		"string_contains":     true,
		"string_compare":      true,
		"string_in_operator":  true,
		"string_index":        true,
		"var_assignment":      true,
		"while_loop":          true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/rb")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".rb")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			n := parseFile(t, srcPath)
			if *updateGolden {
				if j, err := json.MarshalIndent(n, "", "  "); err == nil {
					os.WriteFile(filepath.Join(outDir, name+".json"), j, 0o644)
				}
			}
			astNode := transformNode(t, n)
			var astBuf bytes.Buffer
			if err := ast.Fprint(&astBuf, astNode); err != nil {
				t.Fatalf("print ast: %v", err)
			}
			mochiCode := printNode(t, astNode)
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *updateGolden {
				os.WriteFile(mochiPath, []byte(mochiCode), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".ast"), astBuf.Bytes(), 0o644)
			}
			gotOut, err := run(mochiCode)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *updateGolden {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := run(string(vmSrc))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}
