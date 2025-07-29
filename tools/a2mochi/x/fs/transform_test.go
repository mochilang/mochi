//go:build slow

package fs_test

import (
	"bytes"
	"encoding/json"
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

func parseFile(t *testing.T, path string) *fs.Program {
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := fs.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return prog
}

func transformSrc(t *testing.T, p *fs.Program) string {
	node, err := fs.Transform(p)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	code, err := fs.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	return code
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
		"append_builtin":     true,
		"avg_builtin":        true,
		"basic_compare":      true,
		"cast_string_to_int": true,
		"print_hello":        true,
		"sum_builtin":        true,
		"for_loop":           true,
		"len_builtin":        true,
		"len_string":         true,
		"map_index":          true,
		"if_else":            true,
		"while_loop":         true,
		"unary_neg":          true,
		"closure":            true,
		"fun_call":           true,
		"binary_precedence":  true,
		"bool_chain":         true,
		"break_continue":     false,
		"map_membership":     true,
		"map_int_key":        true,
		"string_concat":      true,
		"min_max_builtin":    true,
		"membership":         true,
		"typed_let":          true,
		"typed_var":          true,
		"string_index":       true,
		"string_contains":    true,
		"var_assignment":     true,
		"user_type_literal":  true,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "fs")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".fs")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			prog := parseFile(t, src)
			j, err := json.Marshal(prog)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			var p2 fs.Program
			if err := json.Unmarshal(j, &p2); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			code := transformSrc(t, &p2)
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *updateGolden {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *updateGolden {
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
