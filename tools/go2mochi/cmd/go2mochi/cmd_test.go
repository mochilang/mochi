package main

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

var update = flag.Bool("update", false, "update golden files")

func TestCLI(t *testing.T) {
	root := findRepoRoot(t)

	bin := filepath.Join(t.TempDir(), "go2mochi")
	buildCmd := exec.Command("go", "build", "-o", bin, ".")
	if out, err := buildCmd.CombinedOutput(); err != nil {
		t.Fatalf("build cli: %v\n%s", err, out)
	}

	files, err := filepath.Glob(filepath.Join(root, "tests", "compiler", "go", "*.go.out"))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no test files found")
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".go.out")
		t.Run(name, func(t *testing.T) {
			cmd := exec.Command(bin, src)
			out, err := cmd.Output()
			outPath := filepath.Join(root, "tests", "compiler", "go", name+".mochi.out")
			errPath := filepath.Join(root, "tests", "compiler", "go", name+".mochi.error")

			if err != nil {
				ee, ok := err.(*exec.ExitError)
				if !ok {
					t.Fatalf("exec error: %v", err)
				}
				if *update {
					os.WriteFile(errPath, normalizeOutput(root, ee.Stderr), 0644)
				}
				want, readErr := os.ReadFile(errPath)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(root, ee.Stderr); !bytes.Equal(got, normalizeOutput(root, want)) {
					t.Errorf("error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}

			if *update {
				os.WriteFile(outPath, normalizeOutput(root, out), 0644)
			}
			want, readErr := os.ReadFile(outPath)
			if readErr != nil {
				t.Fatalf("missing golden output: %v", readErr)
			}
			if got := normalizeOutput(root, out); !bytes.Equal(got, normalizeOutput(root, want)) {
				t.Errorf("golden mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
			}
		})
	}
}

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

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}
