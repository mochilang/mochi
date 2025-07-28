//go:build slow

package rb_test

import (
	"bytes"
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

	"mochi/tools/a2mochi/x/rb"
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
	t.Skip("Ruby converter not fully implemented")

	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/rb", "*.rb")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"append_builtin":     true,
		"basic_compare":      true,
		"break_continue":     true,
		"cast_string_to_int": true,
		"print_hello":        true,
		"avg_builtin":        true,
		"sum_builtin":        true,
		"for_loop":           true,
		"len_builtin":        true,
		"len_string":         true,
		"map_index":          true,
		"if_else":            true,
		"while_loop":         true,
		"unary_neg":          true,
		"membership":         true,
		"map_membership":     true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/rb")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".rb")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			n, err := rb.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			srcOut, err := rb.ConvertSource(n)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			astNode, err := rb.Convert(n)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, got, 0644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(srcOut), 0644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			var buf bytes.Buffer
			if err := ast.Fprint(&buf, astNode); err != nil {
				t.Fatalf("print: %v", err)
			}
			mochiCode := buf.String()
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(mochiCode), 0644)
			}
			gotOut, err := runMochi(mochiCode)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0644)
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
