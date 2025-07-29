//go:build slow

package rkt_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	rkt "mochi/tools/a2mochi/x/rkt"
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

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "rkt", "*.rkt")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"print_hello":         true,
		"append_builtin":      true,
		"fun_call":            true,
		"fun_three_args":      true,
		"unary_neg":           true,
		"let_and_print":       true,
		"for_loop":            true,
		"for_list_collection": true,
		"basic_compare":       true,
		"string_concat":       true,
		"list_index":          true,
		"bool_chain":          true,
		"if_then_else":        true,
		"short_circuit":       true,
		"values_builtin":      true,
		"sum_builtin":         true,
		"min_max_builtin":     true,
		"string_compare":      true,
		"membership":          true,
		"str_builtin":         true,
		"math_ops":            true,
		"string_contains":     true,
		"for_map_collection":  true,
		"len_builtin":         true,
		"len_map":             true,
		"len_string":          true,
		"count_builtin":       true,
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "rkt")
	os.MkdirAll(outDir, 0o755)

	for _, file := range files {
		name := strings.TrimSuffix(filepath.Base(file), ".rkt")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := rkt.Parse(string(src))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			tree, err := rkt.Transform(prog)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			mochiSrc, err := rkt.Print(tree)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *updateGolden {
				os.WriteFile(astPath, []byte(tree.String()), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(mochiSrc), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := tree.String()
			if strings.TrimSpace(string(want)) != strings.TrimSpace(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			gotOut, err := runMochi(mochiSrc)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *updateGolden {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmCode, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut, err := runMochi(string(vmCode))
			if err != nil {
				t.Fatalf("run vm: %v", err)
			}
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}
