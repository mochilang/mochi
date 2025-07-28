//go:build slow

package gox_test

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

	gox "mochi/tools/a2mochi/x/go"
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
	pattern := filepath.Join(root, "tests/transpiler/x/go", "*.go")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"print_hello":    true,
		"let_and_print":  true,
		"if_else":        true,
		"var_assignment": true,
		"len_string":     true,
		"string_concat":  true,
		"str_builtin":    true,
		"unary_neg":      true,
		"for_loop":       true,
		"while_loop":     true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/go")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".go")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := gox.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := gox.Convert(node)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0o644)
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Skipf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			code, err := gox.ConvertSource(node)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut, err := runMochi(code)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
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
