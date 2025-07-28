package gox_test

import (
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	gox "mochi/tools/a2mochi/x/go"
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

func TestConvert_Golden(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/go", "*.go")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	allowed := map[string]bool{
		"avg_builtin":         true,
		"binary_precedence":   true,
		"bool_chain":          true,
		"count_builtin":       true,
		"for_list_collection": true,
		"for_loop":            true,
		"fun_call":            true,
		"fun_three_args":      true,
		"go_auto":             true,
		"len_builtin":         true,
		"len_map":             true,
		"len_string":          true,
		"math_ops":            true,
		"nested_function":     true,
		"print_hello":         true,
		"pure_fold":           true,
		"python_auto":         true,
		"short_circuit":       true,
		"str_builtin":         true,
		"string_compare":      true,
		"string_concat":       true,
		"substring_builtin":   true,
		"sum_builtin":         true,
		"tail_recursion":      true,
		"test_block":          true,
		"unary_neg":           true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/go")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".go")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			node, err := gox.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := gox.Convert(node)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0644)
			}
			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Skipf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
