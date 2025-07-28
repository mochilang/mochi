//go:build slow

package ruby_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/a2mochi/x/ruby"
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

	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/rb", "*.rb")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"break_continue":     true,
		"cast_string_to_int": true,
		"print_hello":        true,
		"len_string":         true,
		"map_index":          true,
		"list_index":         true,
		"list_assign":        true,
		"map_assign":         true,
		"string_concat":      true,
		"if_else":            true,
		"while_loop":         true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/rb")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".rb")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			n, err := ruby.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			srcOut, err := ruby.ConvertSource(n)
			if err != nil {
				t.Fatalf("convert source: %v", err)
			}
			astNode, err := ruby.Convert(n)
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			astPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(astPath, got, 0644)
				os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(srcOut), 0644)
			}
			want, err := os.ReadFile(astPath)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			if string(got) != string(want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, want)
			}
		})
	}
}
