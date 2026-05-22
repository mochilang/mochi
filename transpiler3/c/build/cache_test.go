package build

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"
	"time"
)

// TestCacheHit checks that a second Driver.Build with identical
// inputs against an isolated CacheDir hits the cache and skips cc.
// The cc-skip is observed indirectly: the second call sets
// d.CacheHit=true and completes faster than a fresh build by more
// than a multiple that comfortably exceeds noise (10x; cc on hello
// is ~250-500ms, copyFile is ~0.5ms).
func TestCacheHit(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX host-cc discovery only")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello", "hello.mochi")
	cacheDir := t.TempDir()

	first := &Driver{CacheDir: cacheDir}
	out1 := filepath.Join(t.TempDir(), "hello")
	t0 := time.Now()
	if err := first.Build(src, out1, "", ""); err != nil {
		t.Fatalf("first build: %v", err)
	}
	d1 := time.Since(t0)
	if first.CacheHit {
		t.Fatalf("first build should be a cache miss")
	}

	second := &Driver{CacheDir: cacheDir}
	out2 := filepath.Join(t.TempDir(), "hello2")
	t1 := time.Now()
	if err := second.Build(src, out2, "", ""); err != nil {
		t.Fatalf("second build: %v", err)
	}
	d2 := time.Since(t1)
	if !second.CacheHit {
		t.Fatalf("second build should hit cache; got miss")
	}
	if _, err := os.Stat(out2); err != nil {
		t.Fatalf("cache-hit output not on disk: %v", err)
	}
	if d2 >= d1 {
		t.Fatalf("cache hit (%s) not faster than miss (%s)", d2, d1)
	}
}

// TestCacheInvalidatesOnSourceChange checks that editing the source
// file after a build forces a recompile rather than serving the
// stale cached binary.
func TestCacheInvalidatesOnSourceChange(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX host-cc discovery only")
	}
	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "p.mochi")
	if err := os.WriteFile(src, []byte("print(\"first\")\n"), 0o644); err != nil {
		t.Fatalf("write source v1: %v", err)
	}
	cacheDir := t.TempDir()

	d := &Driver{CacheDir: cacheDir}
	out := filepath.Join(srcDir, "p")
	if err := d.Build(src, out, "", ""); err != nil {
		t.Fatalf("build v1: %v", err)
	}
	if d.CacheHit {
		t.Fatalf("v1 should be a cache miss")
	}

	if err := os.WriteFile(src, []byte("print(\"second\")\n"), 0o644); err != nil {
		t.Fatalf("write source v2: %v", err)
	}
	d = &Driver{CacheDir: cacheDir}
	if err := d.Build(src, out, "", ""); err != nil {
		t.Fatalf("build v2: %v", err)
	}
	if d.CacheHit {
		t.Fatalf("v2 should be a cache miss after source edit")
	}
}

// TestCacheBypassWithKeepEmit checks that KeepEmit=true disables
// the cache entirely (both lookup and store), because the cache
// stores only the binary and a KeepEmit caller wants the C source
// too.
func TestCacheBypassWithKeepEmit(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX host-cc discovery only")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello", "hello.mochi")
	cacheDir := t.TempDir()

	a := &Driver{CacheDir: cacheDir, KeepEmit: true}
	outA := filepath.Join(t.TempDir(), "hello")
	if err := a.Build(src, outA, "", ""); err != nil {
		t.Fatalf("KeepEmit build A: %v", err)
	}
	if a.CacheHit {
		t.Fatalf("KeepEmit build should never report cache hit")
	}

	b := &Driver{CacheDir: cacheDir, KeepEmit: true}
	outB := filepath.Join(t.TempDir(), "hello")
	if err := b.Build(src, outB, "", ""); err != nil {
		t.Fatalf("KeepEmit build B: %v", err)
	}
	if b.CacheHit {
		t.Fatalf("KeepEmit build should never report cache hit (second time)")
	}
}
