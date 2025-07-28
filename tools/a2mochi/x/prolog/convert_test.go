package prolog_test

import (
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	prolog "mochi/tools/a2mochi/x/prolog"
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
	pattern := filepath.Join(root, "tests/transpiler/x/pl", "*.pl")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"avg_builtin":        true,
		"print_hello":        true,
		"unary_neg":          true,
		"for_loop":           true,
		"len_builtin":        true,
		"sum_builtin":        true,
		"let_and_print":      true,
		"math_ops":           true,
		"fun_call":           true,
		"fun_three_args":     true,
		"basic_compare":      true,
		"if_then_else":       true,
		"if_else":            true,
		"while_loop":         true,
		"append_builtin":     true,
		"binary_precedence":  true,
		"cast_string_to_int": true,
		"len_string":         true,
		"string_concat":      true,
		"string_index":       true,
		"slice":              true,
		"string_compare":     true,
		"list_index":         true,
		"string_contains":    true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/prolog")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".pl")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			src, err := prolog.ConvertSource(string(data))
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			node, err := prolog.Convert(string(data))
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, []byte(node.String()), 0o644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0o644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			got := []byte(node.String())
			if string(want) != string(got) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
