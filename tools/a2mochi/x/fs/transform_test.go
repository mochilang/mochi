//go:build slow

package fs_test

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

	"mochi/tools/a2mochi/x/fs"
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

func parseFile(path string) (*fs.Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := fs.Parse(string(data))
	if err != nil {
		return nil, err
	}
	return prog, nil
}

func transformSrc(p *fs.Program) (string, error) {
	node, err := fs.Transform(p)
	if err != nil {
		return "", err
	}
	code, err := fs.Print(node)
	if err != nil {
		return "", err
	}
	return code, nil
}

func TestTransformGolden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "fs", "*.fs")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"avg_builtin":         true,
		"binary_precedence":   true,
		"basic_compare":       true,
		"bool_chain":          false,
		"cast_string_to_int":  true,
		"closure":             false,
		"fun_call":            false,
		"len_builtin":         true,
		"len_string":          true,
		"let_and_print":       true,
		"list_index":          true,
		"map_index":           true,
		"map_int_key":         true,
		"map_membership":      true,
		"if_else":             true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"membership":          true,
		"min_max_builtin":     true,
		"print_hello":         true,
		"str_builtin":         true,
		"string_concat":       true,
		"string_contains":     true,
		"string_in_operator":  true,
		"string_index":        true,
		"sum_builtin":         true,
		"typed_let":           false,
		"typed_var":           false,
		"break_continue":      false,
		"unary_neg":           true,
		"user_type_literal":   true,
		"var_assignment":      true,
		"while_loop":          true,
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "fs")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".fs")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			prog, err := parseFile(src)
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("parse: %v", err)
				return
			}

			code, err := transformSrc(prog)
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("transform: %v", err)
				return
			}

			gotOut, err := runMochi(code)
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("run: %v", err)
				return
			}

			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte("missing vm source"), 0o644)
				}
				t.Errorf("missing vm source: %v", err)
				return
			}
			wantOut, err := runMochi(string(vmSrc))
			if err != nil {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0o644)
				}
				t.Errorf("run vm: %v", err)
				return
			}
			if *updateGolden {
				mochiPath := filepath.Join(outDir, name+".mochi")
				os.WriteFile(mochiPath, []byte(code), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}

			if !bytes.Equal(gotOut, wantOut) {
				if *updateGolden {
					os.WriteFile(filepath.Join(outDir, name+".error"), []byte("output mismatch"), 0o644)
				}
				t.Errorf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
				return
			}

			if *updateGolden {
				os.Remove(filepath.Join(outDir, name+".error"))
			}
		})
	}
}

func updateReadme() {
	fs.UpdateReadme()
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
