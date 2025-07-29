//go:build slow

package ocaml_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

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

func testFile(t *testing.T, root, outDir, srcPath string) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".ml")
	errPath := filepath.Join(outDir, name+".error")
	os.Remove(errPath)

	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := ocaml.Parse(string(data))
	if err != nil {
		os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	node, err := ocaml.Transform(prog)
	if err != nil {
		os.WriteFile(errPath, []byte("transform: "+err.Error()), 0o644)
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
		os.WriteFile(errPath, []byte("ast mismatch"), 0o644)
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}

	code, err := ocaml.Print(node)
	if err != nil {
		os.WriteFile(errPath, []byte("print: "+err.Error()), 0o644)
		t.Fatalf("print: %v", err)
	}
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *update {
		os.WriteFile(mochiPath, []byte(code), 0o644)
	}
	gotOut, err := run(code)
	if err != nil {
		os.WriteFile(errPath, []byte("run: "+err.Error()), 0o644)
		t.Fatalf("run: %v", err)
	}
	if *update {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
	}
	vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
	if err != nil {
		t.Fatalf("missing vm source: %v", err)
	}
	wantOut, err := run(string(vmSrc))
	if err != nil {
		t.Fatalf("run vm: %v", err)
	}
	if !bytes.Equal(gotOut, wantOut) {
		os.WriteFile(errPath, []byte(fmt.Sprintf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)), 0o644)
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
	}
	os.Remove(errPath)
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
		"tail_recursion":      true,
		"avg_builtin":         true,
		"let_and_print":       true,
		"var_assignment":      true,
		"unary_neg":           true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"test_block":          true,
		"list_index":          true,
		"append_builtin":      true,
		"bool_chain":          true,
		"if_else":             true,
		"fun_call":            true,
		"fun_expr_in_let":     true,
		"fun_three_args":      true,
		"math_ops":            true,
		"len_map":             true,
		"str_builtin":         true,
		"min_max_builtin":     true,
		"membership":          true,
		"map_in_operator":     true,
		"map_membership":      true,
		"string_contains":     true,
		"string_in_operator":  true,
		"string_prefix_slice": true,
		"typed_let":           false,
		"typed_var":           false,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".ml")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			testFile(t, root, outDir, srcPath)
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "ocaml")
	pattern := filepath.Join(srcDir, "*.ml")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".ml")
		mark := "[ ]"
		outFile := filepath.Join(outDir, name+".out")
		if data, err := os.ReadFile(outFile); err == nil {
			vmSrc, err2 := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err2 == nil {
				wantOut, err2 := run(string(vmSrc))
				if err2 == nil && bytes.Equal(bytes.TrimSpace(data), wantOut) {
					compiled++
					mark = "[x]"
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi OCaml Converter\n\n")
	buf.WriteString("This directory holds helpers and golden files for converting OCaml programs in `tests/transpiler/x/ocaml` into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	buf.WriteString("Date: " + ts + "\n\n")
	buf.WriteString("Currently " + strconv.Itoa(compiled) + " examples are exercised by the tests.\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "ocaml", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
