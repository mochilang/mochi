//go:build slow

package lua_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"

	"mochi/tools/a2mochi/x/lua"
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

func parseFile(t *testing.T, path string) *lua.Program {
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	prog, err := lua.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return prog
}

func transformAndPrint(t *testing.T, p *lua.Program) (*ast.Node, string) {
	node, err := lua.Transform(p)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	src, err := lua.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	return node, src
}

func run(t *testing.T, src string) []byte {
	out, err := runMochi(src)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	return out
}

func TestTransformGolden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/lua", "*.lua")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"append_builtin":      true,
		"avg_builtin":         true,
		"basic_compare":       true,
		"binary_precedence":   true,
		"bool_chain":          true,
		"break_continue":      true,
		"cast_string_to_int":  true,
		"cast_struct":         true,
		"count_builtin":       true,
		"cross_join_triple":   true,
		"fun_call":            true,
		"fun_expr_in_let":     true,
		"fun_three_args":      true,
		"if_else":             true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"let_and_print":       true,
		"len_builtin":         true,
		"len_map":             true,
		"len_string":          true,
		"list_assign":         true,
		"list_index":          true,
		"list_nested_assign":  true,
		"map_assign":          true,
		"map_index":           true,
		"map_nested_assign":   true,
		"print_hello":         true,
		"pairs_loop":          true,
		"for_loop":            true,
		"for_list_collection": true,
		"for_map_collection":  true,
		"string_concat":       true,
		"string_compare":      true,
		"string_contains":     true,
		"string_index":        true,
		"string_in_operator":  true,
		"in_operator":         true,
		"substring_builtin":   true,
		"sum_builtin":         true,
		"tail_recursion":      true,
		"str_builtin":         true,
		"unary_neg":           true,
		"var_assignment":      true,
		"values_builtin":      true,
		"while_loop":          true,
		"map_int_key":         true,
		"map_literal_dynamic": true,
		"test_block":          true,
		"user_type_literal":   true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/lua")
	os.MkdirAll(outDir, 0o755)
	if matches, _ := filepath.Glob(filepath.Join(outDir, "*.ast")); len(matches) == 0 && !*updateGolden {
		t.Skip("golden files not present")
	}

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".lua")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			prog := parseFile(t, srcPath)
			node, src := transformAndPrint(t, prog)

			got := []byte(node.String())
			astPath := filepath.Join(outDir, name+".ast")
			if *updateGolden {
				os.WriteFile(astPath, got, 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}

			mochiPath := filepath.Join(outDir, name+".mochi")
			if *updateGolden {
				os.WriteFile(mochiPath, []byte(src), 0o644)
			}
			gotOut := run(t, src)
			if *updateGolden {
				os.WriteFile(filepath.Join(outDir, name+".out"), gotOut, 0o644)
			}
			vmSrc, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".mochi"))
			if err != nil {
				t.Fatalf("missing vm source: %v", err)
			}
			wantOut := run(t, string(vmSrc))
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", gotOut, wantOut)
			}
		})
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests/transpiler/x/lua")
	outDir := filepath.Join(root, "tests/a2mochi/x/lua")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.lua"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".lua")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc := time.FixedZone("GMT+7", 7*60*60)
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Lua Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", ts)
	buf.WriteString("This directory contains helpers and golden files for converting Lua programs\nunder `tests/transpiler/x/lua` back into Mochi code.\n\n")
	buf.WriteString("Supported examples:\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools/a2mochi/x/lua/README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
