//go:build slow

package ocaml_test

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

	ocaml "mochi/tools/a2mochi/x/ocaml"
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

func mustRun(t *testing.T, src string) []byte {
	t.Helper()
	out, err := run(src)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	return out
}

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}

	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "ocaml", "*.ml")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"print_hello":         true,
		"len_builtin":         true,
		"len_string":          true,
		"string_index":        true,
		"substring_builtin":   true,
		"basic_compare":       true,
		"binary_precedence":   true,
		"string_concat":       true,
		"string_compare":      true,
		"sum_builtin":         true,
		"avg_builtin":         true,
		"let_and_print":       true,
		"var_assignment":      true,
		"unary_neg":           true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"test_block":          true,
		"list_index":          true,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".ml")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := ocaml.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			node, err := ocaml.Transform(prog)
			if err != nil {
				t.Fatalf("transform: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := node.String()
			if strings.TrimSpace(string(want)) != strings.TrimSpace(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			code, err := ocaml.Print(node)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			mochiPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(mochiPath, []byte(code), 0o644)
			}
			gotOut := mustRun(t, code)
			if *update {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut := mustRun(t, string(vmSrc))
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}
