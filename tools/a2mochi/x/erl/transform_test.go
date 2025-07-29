//go:build slow

package erl_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	erl "mochi/tools/a2mochi/x/erl"
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

func transformFile(t *testing.T, root, outDir, srcPath string) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".erl")
	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	code := string(data)
	prog, err := erl.Parse(code)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	node, err := erl.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	src, err := erl.Print(node)
	if err != nil {
		t.Fatalf("print source: %v", err)
	}
	var buf bytes.Buffer
	if err := ast.Fprint(&buf, node); err != nil {
		t.Fatalf("print: %v", err)
	}
	t.Log(buf.String())
	mochiPath := filepath.Join(outDir, name+".mochi")
	if *update {
		os.WriteFile(mochiPath, []byte(src), 0o644)
	}
	gotOut, err := runMochi(src)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if *update {
		os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
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
}

func TestTransform_Golden(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skipf("escript not installed: %v", err)
	}
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "erl", "*.erl")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allow := map[string]bool{
		"append_builtin":      true,
		"avg_builtin":         true,
		"basic_compare":       true,
		"binary_precedence":   true,
		"print_hello":         true,
		"count_builtin":       true,
		"len_builtin":         true,
		"len_string":          true,
		"len_map":             true,
		"map_index":           true,
		"cast_string_to_int":  true,
		"map_int_key":         true,
		"map_membership":      true,
		"min_max_builtin":     true,
		"values_builtin":      true,
		"string_concat":       true,
		"string_contains":     true,
		"string_index":        true,
		"string_compare":      true,
		"string_prefix_slice": true,
		"substring_builtin":   true,
		"sum_builtin":         true,
		"bool_chain":          true,
		"short_circuit":       true,
		"unary_neg":           true,
		"if_else":             true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"closure":             true,
		"for_list_collection": true,
		"for_loop":            true,
		"for_map_collection":  true,
		"fun_call":            true,
		"fun_three_args":      true,
		"let_and_print":       true,
		"list_index":          true,
		"var_assignment":      true,
		"map_assign":          true,
		"map_literal_dynamic": true,
		"list_assign":         true,
		"list_nested_assign":  true,
		"in_operator":         true,
		"string_in_operator":  true,
		"membership":          true,
		"map_in_operator":     true,
		"map_nested_assign":   true,
		"slice":               true,
		"record_assign":       false,
	}
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "erl")
	os.MkdirAll(outDir, 0o755)
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".erl")
		if !allow[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			transformFile(t, root, outDir, srcPath)
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "erl")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "erl")
	pattern := filepath.Join(srcDir, "*.erl")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".erl")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	now := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Erlang Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", now)
	buf.WriteString("This directory stores golden files for the Erlang to Mochi converter.\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "erl", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
