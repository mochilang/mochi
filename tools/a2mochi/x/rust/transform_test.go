//go:build slow

package rust_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/ast"

	rust "mochi/tools/a2mochi/x/rust"
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

func parseProgram(t *testing.T, src []byte) (*rust.Program, rust.ASTNode) {
	prog, err := rust.Parse(string(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	data, err := rust.MarshalAST(prog.AST)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var node rust.ASTNode
	if err := json.Unmarshal(data, &node); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	prog.AST = &node
	return prog, node
}

func transformSource(t *testing.T, src string, astNode *rust.ASTNode) (*ast.Node, string) {
	prog := &rust.Program{Source: src, AST: astNode}
	node, err := rust.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}
	var buf bytes.Buffer
	if err := ast.Fprint(&buf, node); err != nil {
		t.Fatalf("print: %v", err)
	}
	code, err := rust.ConvertSourceAST(src, astNode)
	if err != nil {
		t.Fatalf("generate mochi: %v", err)
	}
	return node, code
}

func runMochi(t *testing.T, rootDir, path string) []byte {
	cmd := exec.Command("go", "run", "./cmd/mochi", "run", path)
	cmd.Dir = rootDir
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		t.Fatalf("run mochi: %v\n%s", err, out.String())
	}
	return out.Bytes()
}

func TestTransformGolden(t *testing.T) {
	if _, err := exec.LookPath("rust-analyzer"); err != nil {
		t.Skipf("rust-analyzer not installed: %v", err)
	}

	rootDir := repoRoot(t)
	pattern := filepath.Join(rootDir, "tests/transpiler/x/rs", "*.rs")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(rootDir, "tests/a2mochi/x/rust")
	os.MkdirAll(outDir, 0o755)

	allowed := map[string]bool{
		"print_hello":         true,
		"let_and_print":       true,
		"append_builtin":      true,
		"for_loop":            true,
		"while_loop":          true,
		"basic_compare":       true,
		"len_string":          true,
		"fun_call":            true,
		"fun_three_args":      true,
		"bool_chain":          true,
		"math_ops":            true,
		"var_assignment":      true,
		"typed_var":           true,
		"typed_let":           true,
		"unary_neg":           true,
		"cast_string_to_int":  true,
		"count_builtin":       true,
		"list_index":          true,
		"list_assign":         true,
		"list_set_ops":        true,
		"if_else":             true,
		"if_then_else":        true,
		"if_then_else_nested": true,
	}

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".rs")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}

			_, astNode := parseProgram(t, data)
			node, code := transformSource(t, string(data), &astNode)

			var astBuf bytes.Buffer
			if err := ast.Fprint(&astBuf, node); err != nil {
				t.Fatalf("print: %v", err)
			}
			got := astBuf.Bytes()
			outPath := filepath.Join(outDir, name+".ast")

			mochiPath := filepath.Join(outDir, name+".mochi")
			if err := os.WriteFile(mochiPath, []byte(code), 0644); err != nil {
				t.Fatalf("write mochi: %v", err)
			}

			out := runMochi(t, rootDir, mochiPath)
			outFile := filepath.Join(outDir, name+".out")
			if err := os.WriteFile(outFile, out, 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}

			refOutPath := filepath.Join(rootDir, "tests/vm/valid", name+".mochi.out")
			refOut, err := os.ReadFile(refOutPath)
			if err != nil {
				t.Fatalf("read ref: %v", err)
			}
			if string(out) != string(refOut) {
				t.Fatalf("output mismatch\n--- Got ---\n%s\n--- Want ---\n%s", out, refOut)
			}

			if *update {
				os.WriteFile(outPath, got, 0644)
			}

			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
