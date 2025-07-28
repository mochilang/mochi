package c_test

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

	c "mochi/tools/a2mochi/x/c"
)

var update = flag.Bool("update", false, "update golden files")

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func TestConvertSimple(t *testing.T) {
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not installed")
	}
	src := "#include <stdio.h>\nint main(){printf(\"hi\\n\");}"
	code, err := c.ConvertSource(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if code == "" {
		t.Fatalf("empty output")
	}
}

func TestConvert_Golden(t *testing.T) {
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "c", "*.c")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"append_builtin":      true,
		"basic_compare":       true,
		"cast_string_to_int":  true,
		"print_hello":         true,
		"avg_builtin":         true,
		"sum_builtin":         true,
		"for_loop":            true,
		"len_builtin":         true,
		"len_string":          true,
		"map_index":           true,
		"if_else":             true,
		"while_loop":          true,
		"unary_neg":           true,
		"let_and_print":       true,
		"list_index":          true,
		"fun_three_args":      true,
		"fun_expr_in_let":     true,
		"string_concat":       true,
		"string_compare":      true,
		"string_index":        true,
		"string_prefix_slice": true,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "c")
	os.MkdirAll(outDir, 0o755)
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".c")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			astNode, err := c.Convert(string(data))
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			var buf bytes.Buffer
			if err := ast.Fprint(&buf, astNode); err != nil {
				t.Fatalf("print: %v", err)
			}
			code := buf.String()
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(astNode.String()), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(code), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Skipf("missing golden: %v", err)
			}
			if strings.TrimSpace(string(want)) != strings.TrimSpace(astNode.String()) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", astNode.String(), want)
			}

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
