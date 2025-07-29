//go:build slow

package prolog_test

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
		"avg_builtin":        true,
		"print_hello":        true,
		"for_loop":           true,
		"len_builtin":        true,
		"sum_builtin":        true,
		"let_and_print":      true,
		"basic_compare":      true,
		"if_else":            true,
		"while_loop":         true,
		"append_builtin":     true,
		"cast_string_to_int": true,
		"len_string":         true,
		"string_concat":      true,
		"string_compare":     true,
		"bool_chain":         true,
		"str_builtin":        true,
		"var_assignment":     true,
		"len_map":            true,
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
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				os.WriteFile(filepath.Join(outputDir, name+".mochi"), []byte(code), 0o644)
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
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}
