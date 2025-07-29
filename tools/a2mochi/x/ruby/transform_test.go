//go:build slow

package ruby_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
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

func parseFile(path string) (*ruby.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read src: %w", err)
	}
	node, err := ruby.Parse(string(data))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	return node, nil
}

func transformNode(n *ruby.Node) (*ast.Node, error) {
	node, err := ruby.Transform(n)
	if err != nil {
		return nil, fmt.Errorf("transform: %w", err)
	}
	return node, nil
}

func printNode(n *ast.Node) (string, error) {
	src, err := ruby.Print(n)
	if err != nil {
		return "", fmt.Errorf("print: %w", err)
	}
	return src, nil
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
		"append_builtin":      true,
		"avg_builtin":         true,
		"basic_compare":       true,
		"break_continue":      true,
		"cast_string_to_int":  true,
		"count_builtin":       true,
		"for_loop":            true,
		"for_list_collection": true,
		"for_map_collection":  true,
		"if_else":             true,
		"len_builtin":         true,
		"len_map":             true,
		"len_string":          true,
		"let_and_print":       true,
		"list_assign":         true,
		"list_index":          true,
		"list_nested_assign":  true,
		"map_assign":          true,
		"map_index":           true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"map_membership":      true,
		"map_in_operator":     true,
		"map_nested_assign":   true,
		"string_prefix_slice": true,
		"substring_builtin":   true,
		"membership":          true,
		"print_hello":         true,
		"sum_builtin":         true,
		"str_builtin":         true,
		"string_concat":       true,
		"string_contains":     true,
		"string_compare":      true,
		"string_in_operator":  true,
		"string_index":        true,
		"unary_neg":           true,
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
			errPath := filepath.Join(outDir, name+".error")
			if *updateGolden {
				os.Remove(errPath)
			}

			n, err := parseFile(srcPath)
			if err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte(err.Error()+"\n"), 0o644)
				}
				t.Fatalf("%v", err)
			}
			if *updateGolden {
				if j, err2 := json.MarshalIndent(n, "", "  "); err2 == nil {
					os.WriteFile(filepath.Join(outDir, name+".json"), j, 0o644)
				}
			}

			astNode, err := transformNode(n)
			if err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte(err.Error()+"\n"), 0o644)
				}
				t.Fatalf("%v", err)
			}
			var astBuf bytes.Buffer
			if err := ast.Fprint(&astBuf, astNode); err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte("print ast: "+err.Error()+"\n"), 0o644)
				}
				t.Fatalf("print ast: %v", err)
			}
			mochiCode, err := printNode(astNode)
			if err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte(err.Error()+"\n"), 0o644)
				}
				t.Fatalf("%v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *updateGolden {
				os.WriteFile(mochiPath, []byte(mochiCode), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".ast"), astBuf.Bytes(), 0o644)
			}
			gotOut, err := run(mochiCode)
			if err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte("run: "+err.Error()+"\n"), 0o644)
				}
				t.Fatalf("run: %v", err)
			}
			if *updateGolden {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".mochi"))
			if err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte("missing vm source: "+err.Error()+"\n"), 0o644)
				}
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := run(string(vmSrc))
			if err != nil {
				if *updateGolden {
					os.WriteFile(errPath, []byte("run vm: "+err.Error()+"\n"), 0o644)
				}
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				if *updateGolden {
					os.WriteFile(errPath, []byte("output mismatch\n"+"Got: "+string(gotOut)+"\nWant: "+string(wantOut)+"\n"), 0o644)
				}
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
			if *updateGolden {
				os.Remove(errPath)
			}
		})
	}
	if *updateGolden {
		updateReadme()
	}
}

func updateReadme() {
	ruby.UpdateReadmeForTests()
}

func TestMain(m *testing.M) {
	code := m.Run()
	if *updateGolden {
		updateReadme()
	}
	os.Exit(code)
}
