package golden

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// update is a flag to overwrite golden files with new output.
var update = flag.Bool("update", false, "update golden files")

// Runner defines a test processor function.
type Runner func(srcPath string) ([]byte, error)

// Run executes golden file-based tests.
// It compares processed output of `*.srcExt` files with corresponding `*.goldenExt` files.
func Run(t *testing.T, dir, srcExt, goldenExt string, fn Runner) {
	rootDir := findRepoRoot(t)
	pattern := filepath.Join(rootDir, dir, "*"+srcExt)

	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("failed to list %s files in %s: %v", srcExt, dir, err)
	}
	if len(files) == 0 {
		t.Fatalf("no test files found: %s", pattern)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), srcExt)
		wantPath := filepath.Join(rootDir, dir, name+goldenExt)

		t.Run(name, func(t *testing.T) {
			got, err := fn(src)
			if err != nil {
				t.Fatalf("process error: %v", err)
			}
			if got == nil {
				t.Fatal("got nil output")
			}
			got = normalizeOutput(rootDir, got)

			if *update {
				if err := os.WriteFile(wantPath, got, 0644); err != nil {
					t.Fatalf("failed to write golden: %v", err)
				}
				t.Logf("updated: %s", wantPath)
				return
			}

			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("failed to read golden: %v", err)
			}
			want = bytes.TrimSpace(want)

			if !bytes.Equal(got, want) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+goldenExt, got, want)
			}
		})
	}
}

// findRepoRoot walks up the directory tree to locate the `go.mod` file.
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

// normalizeOutput strips project-specific paths and whitespace for clean diffing.
func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	return []byte(strings.TrimSpace(out))
}
