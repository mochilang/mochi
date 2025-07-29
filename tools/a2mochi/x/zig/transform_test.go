//go:build slow

package zig_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/tools/a2mochi/x/zig"
	"mochi/tools/slt/logic"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t testing.TB) string {
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
	t.Fatalf("go.mod not found")
	return "" // unreachable
}

func runTransform(t *testing.T, srcPath, root, outDir string) {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".zig")

	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}

	prog, err := zig.Parse(string(data))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	node, err := zig.Transform(prog)
	if err != nil {
		t.Fatalf("transform: %v", err)
	}

	src, err := zig.Print(node)
	if err != nil {
		t.Fatalf("print: %v", err)
	}

	astPath := filepath.Join(outDir, name+".ast")
	mochiPath := filepath.Join(outDir, name+".mochi")
	outPath := filepath.Join(outDir, name+".out")

	if *update {
		os.WriteFile(astPath, []byte(node.String()), 0o644)
		os.WriteFile(mochiPath, []byte(src), 0o644)
	}

	want, err := os.ReadFile(astPath)
	if err != nil {
		t.Fatalf("missing golden: %v", err)
	}
	if got := []byte(node.String()); string(want) != string(got) {
		t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
	}

	out, err := logic.RunMochi(src, 5*time.Second)
	if err != nil {
		t.Fatalf("run mochi: %v", err)
	}

	if *update {
		os.WriteFile(outPath, []byte(out), 0o644)
	}

	wantOut, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".out"))
	if err != nil {
		t.Fatalf("missing vm output: %v", err)
	}
	if strings.TrimSpace(out) != strings.TrimSpace(string(wantOut)) {
		t.Fatalf("output mismatch: got %q want %q", strings.TrimSpace(out), strings.TrimSpace(string(wantOut)))
	}
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/zig", "*.zig")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"print_hello":         true,
		"unary_neg":           true,
		"pure_fold":           true,
		"tail_recursion":      true,
		"binary_precedence":   true,
		"avg_builtin":         true,
		"fun_call":            true,
		"for_loop":            true,
		"len_builtin":         true,
		"len_string":          true,
		"bool_chain":          true,
		"let_and_print":       true,
		"math_ops":            true,
		"basic_compare":       true,
		"var_assignment":      true,
		"while_loop":          true,
		"str_builtin":         true,
		"string_concat":       true,
		"substring_builtin":   true,
		"sum_builtin":         true,
		"append_builtin":      true,
		"if_else":             true,
		"string_compare":      true,
		"break_continue":      true,
		"list_index":          true,
		"list_assign":         true,
		"string_index":        true,
		"string_contains":     true,
		"slice":               true,
		"if_then_else":        true,
		"if_then_else_nested": true,
		"for_list_collection": true,
		"string_in_operator":  true,
		"fun_expr_in_let":     true,
		"record_assign":       true,
		"in_operator":         true,
		"count_builtin":       true,
		"short_circuit":       true,
		"test_block":          true,
		"two-sum":             true,
	}

	if _, err := exec.LookPath("zig"); err != nil {
		t.Skipf("zig not installed: %v", err)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/zig")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".zig")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			runTransform(t, srcPath, root, outDir)
		})
	}
}
