//go:build slow

package fortran_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	fortran "mochi/tools/a2mochi/x/fortran"
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
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/fortran", "*.f90")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	allowed := map[string]bool{
		"avg_builtin": true,
		"for_loop":    true,
		"print_hello": true,
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/fortran")
	os.MkdirAll(outDir, 0o755)

	tsOut, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsOut))

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".f90")
		if !allowed[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			os.Setenv("MOCHI_HEADER_TIME", ts)
			node, err := fortran.Parse(string(data))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			astNode, err := fortran.Convert(node)
			os.Unsetenv("MOCHI_HEADER_TIME")
			if err != nil {
				t.Fatalf("convert: %v", err)
			}
			got := []byte(astNode.String())
			outPath := filepath.Join(outDir, name+".ast")
			if *update {
				os.WriteFile(outPath, got, 0o644)
				if src, err := fortran.ConvertSource(node); err == nil {
					os.WriteFile(filepath.Join(outDir, name+".mochi"), []byte(src), 0o644)
				}
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
