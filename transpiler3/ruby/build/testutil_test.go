package build

import (
	"os"
	"path/filepath"
	"testing"
)

// repoRootForTest returns the absolute path to the repo root by walking up
// from the test binary's directory until it finds go.mod.
func repoRootForTest(t *testing.T) string {
	t.Helper()
	dir, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("filepath.Abs: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("go.mod not found from %s", dir)
		}
		dir = parent
	}
}
