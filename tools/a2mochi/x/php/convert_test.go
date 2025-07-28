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
	"mochi/types"

	"mochi/tools/a2mochi/x/php"
)

var update = flag.Bool("update", false, "update golden files")

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
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "php", "*.php")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"unary_neg":          true,
		"print_hello":        true,
		"let_and_print":      true,
		"list_index":         true,
		"avg_builtin":        true,
		"sum_builtin":        true,
		"len_builtin":        true,
		"count_builtin":      true,
		"basic_compare":      true,
		"string_concat":      true,
		"string_compare":     true,
		"binary_precedence":  true,
		"cast_string_to_int": true,
		"bool_chain":         true,
		"len_map":            true,
		"len_string":         true,
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "php")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".php")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			node, err := php.Convert(string(data))
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				code, err := php.ConvertFile(src)
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
