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
	pattern := filepath.Join(root, "tests/transpiler/x/java", "*.java")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"print_hello":    true,
		"unary_neg":      true,
		"let_and_print":  true,
		"var_assignment": true,
		"for_loop":       true,
		"while_loop":     true,
		"string_concat":  true,
		"len_builtin":    true,
		"count_builtin":  true,
		"len_string":     true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/java")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".java")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := java.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := java.Convert(node)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			code, err := java.ConvertSource(node)
			if err != nil {
				t.Fatalf("convert src: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(astNode.String()), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(code), 0o644)
				out, err := runMochi(code)
				if err == nil {
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
			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
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
