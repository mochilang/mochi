package javadbg_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/a2mochi/x/javadbg"
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
	if _, err := exec.LookPath("mochi-javaast"); err != nil {
		t.Skip("mochi-javaast not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "compiler", "java", "*.java.out")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	outDir := filepath.Join(root, "tests", "any2mochi", "java")

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".java.out")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			code, err := javadbg.ConvertSource(string(data))
			if err != nil {
				errPath := filepath.Join(outDir, name+".error")
				if *update {
					os.WriteFile(errPath, []byte(err.Error()), 0644)
				}
				want, e := os.ReadFile(errPath)
				if e != nil {
					t.Fatalf("missing golden error: %v", e)
				}
				if strings.TrimSpace(err.Error()) != strings.TrimSpace(string(want)) {
					t.Fatalf("error mismatch\nGot: %s\nWant: %s", err.Error(), want)
				}
				return
			}
			outPath := filepath.Join(outDir, name+".mochi")
			if *update {
				os.WriteFile(outPath, []byte(code), 0644)
			}
			want, e := os.ReadFile(outPath)
			if e != nil {
				t.Fatalf("missing golden output: %v", e)
			}
			if strings.TrimSpace(code) != strings.TrimSpace(string(want)) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", code, want)
			}
		})
	}
}
