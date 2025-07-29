//go:build slow

package java_test

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

	java "mochi/tools/a2mochi/x/java"
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

func runFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return run(string(data))
}

func TestTransform_Golden(t *testing.T) {
	rootDir := repoRoot(t)
	pattern := filepath.Join(rootDir, "tests/transpiler/x/java", "*.java")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	cases := map[string]bool{
		"print_hello":         true,
		"let_and_print":       true,
		"var_assignment":      true,
		"for_loop":            true,
		"math_ops":            true,
		"string_concat":       true,
		"basic_compare":       true,
		"for_list_collection": true,
		"len_builtin":         true,
		"len_string":          true,
		"break_continue":      true,
		"string_compare":      true,
               "string_contains":     true,
               "string_index":        true,
               "str_builtin":         true,
               "substring_builtin":   true,
               "string_prefix_slice": true,
       }

	outDir := filepath.Join(rootDir, "tests/a2mochi/x/java")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".java")
		if !cases[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			node, err := java.ParseFile(srcPath)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := java.Transform(node)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			code, err := java.Print(astNode)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(astPath, []byte(astNode.String()), 0o644)
				os.WriteFile(mochiPath, []byte(code), 0o644)
				if out, err := runFile(mochiPath); err == nil {
					os.WriteFile(filepath.Join(outDir, name+".out"), out, 0o644)
				}
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if astNode.String() != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", astNode.String(), want)
			}
			// also check runtime output matches vm reference
			gotOut, err := runFile(mochiPath)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			vmSrc, err := os.ReadFile(filepath.Join(rootDir, "tests/vm/valid", name+".mochi"))
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
