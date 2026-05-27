package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase1_2Cache verifies that building the same source twice with an
// explicit CacheDir results in a cache hit on the second call (Phase 1.2).
func TestPhase1_2Cache(t *testing.T) {
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase01", "001_hello", "001_hello.mochi")
	if _, err := os.Stat(src); err != nil {
		t.Skipf("fixture not found: %v", err)
	}

	cacheDir := t.TempDir()
	out1 := filepath.Join(t.TempDir(), "first.escript")
	out2 := filepath.Join(t.TempDir(), "second.escript")

	d := &Driver{CacheDir: cacheDir}

	// First build: cold cache.
	if err := d.Build(src, out1, TargetEscript); err != nil {
		t.Fatalf("first build: %v", err)
	}
	stat1, err := os.Stat(out1)
	if err != nil {
		t.Fatalf("stat out1: %v", err)
	}

	// Cache should now contain exactly one .escript file.
	entries, _ := os.ReadDir(cacheDir)
	var cachedCount int
	for _, e := range entries {
		if filepath.Ext(e.Name()) == ".escript" {
			cachedCount++
		}
	}
	if cachedCount != 1 {
		t.Fatalf("expected 1 cached escript, got %d", cachedCount)
	}

	// Second build: warm cache — should copy from cache.
	if err := d.Build(src, out2, TargetEscript); err != nil {
		t.Fatalf("second build: %v", err)
	}
	stat2, err := os.Stat(out2)
	if err != nil {
		t.Fatalf("stat out2: %v", err)
	}

	if stat1.Size() != stat2.Size() {
		t.Errorf("cached build size mismatch: first=%d second=%d", stat1.Size(), stat2.Size())
	}
}
