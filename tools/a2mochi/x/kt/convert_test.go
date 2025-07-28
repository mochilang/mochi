//go:build slow

package kt_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/a2mochi/x/kt"
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
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skipf("kotlinc not installed: %v", err)
	}

	t.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/kt", "*.kt")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/kt")
	os.MkdirAll(outDir, 0o755)
	allowed := map[string]bool{
		"print_hello":       true,
		"append_builtin":    true,
		"basic_compare":     true,
		"avg_builtin":       true,
		"let_and_print":     true,
		"list_index":        true,
		"while_loop":        true,
		"fun_three_args":    true,
		"binary_precedence": true,
		"bool_chain":        true,
		"fun_call":          true,
		"len_builtin":       true,
		"len_string":        true,
		"string_concat":     true,
		"unary_neg":         true,
		"count_builtin":     true,
		"for_loop":          true,
		"if_else":           true,
		"string_compare":    true,
		"var_assignment":    true,
		"sum_builtin":       true,
	}

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".kt")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			nodes, err := kt.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			src, err := kt.ConvertSource(nodes, string(data))
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			astNode, err := kt.Convert(nodes)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0644)
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
