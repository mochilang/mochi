//go:build slow

package ex_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

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
		"let_and_print":      true,
		"print_hello":        true,
		"unary_neg":          true,
		"count_builtin":      true,
		"sum_builtin":        true,
		"len_builtin":        true,
		"len_string":         true,
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
		"list_assign":        true,
		"string_index":       true,
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
