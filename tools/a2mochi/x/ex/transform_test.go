//go:build slow

package ex_test

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

	ex "mochi/tools/a2mochi/x/ex"
)

type qNode struct {
	Op     string
	Line   int
	Column int
	Value  string
	Args   []qNode
}

func fromRaw(v interface{}) qNode {
	switch x := v.(type) {
	case string:
		return qNode{Op: "string", Value: x}
	case float64:
		return qNode{Op: "number", Value: fmt.Sprintf("%v", x)}
	case []interface{}:
		n := qNode{}
		if len(x) > 0 {
			if s, ok := x[0].(string); ok {
				n.Op = s
			}
		}
		if len(x) > 1 {
			if meta, ok := x[1].([]interface{}); ok {
				for _, m := range meta {
					if p, ok := m.([]interface{}); ok && len(p) == 2 {
						key, _ := p[0].(string)
						if val, ok := p[1].(float64); ok {
							switch key {
							case "line":
								n.Line = int(val)
							case "column":
								n.Column = int(val)
							}
						}
					}
				}
			}
		}
		if len(x) > 2 {
			if args, ok := x[2].([]interface{}); ok {
				for _, a := range args {
					n.Args = append(n.Args, fromRaw(a))
				}
			}
		}
		return n
	default:
		return qNode{Op: "unknown"}
	}
}

func TestTransformBasic(t *testing.T) {
	src := `def main() do
  IO.puts("hi")
end`
	node := transformSource(t, src)
	got := node.String()
	want := "(program\n  (fun main\n    (call print (string hi))\n  )\n  (call main)\n)\n"
	if got != want {
		t.Fatalf("unexpected AST\nGot: %s\nWant: %s", got, want)
	}
}

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

func transformSource(t *testing.T, src string) *ast.Node {
	t.Helper()
	prog, err := ex.Parse(src)
	if err != nil {
		if !strings.Contains(err.Error(), "no functions found") {
			t.Fatalf("parse: %v", err)
		}
		n, err := ex.TransformString(src)
		if err != nil {
			t.Fatalf("transform: %v", err)
		}
		return n
	}
	n, err := ex.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	return n
}

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "transpiler", "x", "ex", "*.exs")
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
		"binary_precedence":  true,
		"bool_chain":         true,
		"break_continue":     true,
		"cast_string_to_int": true,
		"fun_call":           true,
		"fun_three_args":     true,
		"let_and_print":      true,
		"print_hello":        true,
		"unary_neg":          true,
		"count_builtin":      true,
		"exists_builtin":     true,
		// dataset joins not yet supported
		"cross_join":         false,
		"cross_join_triple":  false,
		"sum_builtin":        true,
		"len_builtin":        true,
		"len_string":         true,
		"in_operator":        true,
		"string_concat":      true,
		"string_compare":     true,
		"avg_builtin":        true,
		"min_max_builtin":    true,
		"string_contains":    true,
		"substring_builtin":  true,
		"for_loop":           true,
		"if_else":            true,
		"var_assignment":     true,
		"len_map":            true,
		"map_in_operator":    true,
		"map_membership":     true,
		"map_index":          true,
		"map_assign":         true,
		"map_nested_assign":  true,
		"string_index":       true,
		"list_assign":        true,
		"list_index":         true,
		"list_nested_assign": true,
		"user_type_literal":  true,
		"values_builtin":     true,
	}

	outDir := filepath.Join(root, "tests", "a2mochi", "x", "ex")
	os.MkdirAll(outDir, 0o755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".exs")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			raw, err := ex.ParseAST(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astStruct := fromRaw(raw)
			_ = astStruct // structure used for conversion

			node := transformSource(t, string(data))
			t.Log(node.String())
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

			code, err := ex.Print(node)
			if err != nil {
				t.Fatalf("print: %v", err)
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

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "ex")
	pattern := filepath.Join(srcDir, "*.exs")
	files, _ := filepath.Glob(pattern)
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".exs")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Elixir AST Conversion\n\n")
	buf.WriteString("This directory provides a small converter that turns a subset of Elixir source code into Mochi AST form. The implementation mirrors the Python and TypeScript converters and is mostly regex based.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	buf.WriteString("Date: " + time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04:05") + " GMT+7\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "ex", "README.md"), buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}
