package golden

import (
	"bytes"
	"context"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/tools/db"
)

var update = flag.Bool("update", false, "update golden files")

// Runner defines a test processor function.
type Runner func(srcPath string) ([]byte, error)

// Run executes golden file-based tests and logs to db.GoldenModel.
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
			input, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("failed to read input: %v", err)
			}

			start := time.Now()
			got, err := fn(src)
			dur := time.Since(start)

			model := &db.GoldenModel{
				Name:      name,
				File:      src,
				Input:     string(input),
				Output:    string(got),
				Duration:  dur,
				CreatedAt: time.Now(),
			}

			if err != nil {
				model.Status = "error"
				model.Error = err.Error()
				db.LogGolden(context.Background(), model)
				t.Fatalf("process error: %v", err)
			}
			if got == nil {
				model.Status = "fail"
				model.Error = "got nil output"
				db.LogGolden(context.Background(), model)
				t.Fatal("got nil output")
			}

			got = normalizeOutput(rootDir, got)

			if *update {
				if err := os.WriteFile(wantPath, got, 0644); err != nil {
					t.Fatalf("failed to write golden: %v", err)
				}
				t.Logf("updated: %s", wantPath)
				model.Status = "ok"
				db.LogGolden(context.Background(), model)
				return
			}

			want, err := os.ReadFile(wantPath)
			if err != nil {
				model.Status = "error"
				model.Error = "missing golden: " + err.Error()
				db.LogGolden(context.Background(), model)
				t.Fatalf("failed to read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			model.Output = string(got)

			if !bytes.Equal(got, want) {
				model.Status = "fail"
				model.Error = "golden mismatch"
				db.LogGolden(context.Background(), model)
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+goldenExt, got, want)
				return
			}

			model.Status = "ok"
			db.LogGolden(context.Background(), model)
		})
	}
}

// findRepoRoot walks up to locate the `go.mod` file.
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

// normalizeOutput strips paths for clean diffing.
func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	return []byte(strings.TrimSpace(out))
}
