package resolve

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

// TestCacheRoundTrip writes a PackageBinding to disk and reads it back.
func TestCacheRoundTrip(t *testing.T) {
	dir := t.TempDir()
	c := &Cache{Dir: dir}

	r := &Resolver{Cache: c, GoSumHash: "deadbeef"}
	pb, err := r.Resolve("strings")
	if err != nil {
		t.Fatalf("first resolve: %v", err)
	}
	if pb.GoSumHash != "deadbeef" {
		t.Errorf("go.sum hash = %q", pb.GoSumHash)
	}

	// Drop the in-memory memo to force a disk hit.
	c.memo = nil

	pb2, ok := c.Load("strings", "deadbeef")
	if !ok {
		t.Fatal("cache miss on second load")
	}
	if pb2.Stats() != pb.Stats() {
		t.Errorf("round-trip not equal:\n  in : %s\n  out: %s", pb.Stats(), pb2.Stats())
	}
}

// TestCacheHitUnderOneMillisecond locks in the Phase 2 gate from
// MEP-43: "cache hit on second call under 1 ms". The in-memory memo
// hit is the steady-state cost during a single `mochi build`; the
// disk hit is the cold-start cost (the user's first build after a
// `go.mod` change, or the very first build on a fresh checkout). We
// assert <1 ms on memo and <100 ms on disk; the disk cost is bounded
// by gob decode of the largest stdlib package.
func TestCacheHitUnderOneMillisecond(t *testing.T) {
	dir := t.TempDir()
	c := &Cache{Dir: dir}
	r := &Resolver{Cache: c, GoSumHash: "abc123"}
	if _, err := r.Resolve("strings"); err != nil {
		t.Fatal(err)
	}

	// Best of three: the memo path is sub-microsecond on idle, but
	// scheduler jitter on a loaded CI host can push a single sample
	// over 1 ms even when the steady-state median is far under.
	best := time.Hour
	for i := 0; i < 3; i++ {
		start := time.Now()
		_, ok := c.Load("strings", "abc123")
		elapsed := time.Since(start)
		if !ok {
			t.Fatal("memo miss")
		}
		if elapsed < best {
			best = elapsed
		}
	}
	if best > time.Millisecond {
		t.Errorf("memo hit best-of-3 = %v (gate: <1ms)", best)
	}

	// Disk hit (drop memo): gob-decode of the entire `strings`
	// binding. Loose bound; the gate is the memo path, not this.
	c.memo = nil
	start := time.Now()
	if _, ok := c.Load("strings", "abc123"); !ok {
		t.Fatal("disk miss")
	}
	if elapsed := time.Since(start); elapsed > 100*time.Millisecond {
		t.Errorf("disk hit took %v (bound: <100ms)", elapsed)
	}
}

// TestCacheInvalidationOnGoSumChange exercises the cache key:
// a different go.sum hash must not return the stale entry.
func TestCacheInvalidationOnGoSumChange(t *testing.T) {
	dir := t.TempDir()
	c := &Cache{Dir: dir}
	r := &Resolver{Cache: c, GoSumHash: "v1hash"}
	if _, err := r.Resolve("strings"); err != nil {
		t.Fatal(err)
	}
	if _, ok := c.Load("strings", "v2hash"); ok {
		t.Fatal("cache returned entry for wrong go.sum hash")
	}
	if _, ok := c.Load("strings", "v1hash"); !ok {
		t.Fatal("cache missed for the right go.sum hash")
	}
}

// TestCacheInvalidationOnMochiVersionChange confirms that a cached
// entry written by an older resolver version is rejected.
func TestCacheInvalidationOnMochiVersionChange(t *testing.T) {
	dir := t.TempDir()
	c := &Cache{Dir: dir}
	pb := &PackageBinding{
		ImportPath:   "fake",
		Name:         "fake",
		GoSumHash:    "h",
		MochiVersion: "mep-0043/v0-stale",
	}
	if err := c.Store(pb); err != nil {
		t.Fatal(err)
	}
	c.memo = nil
	if _, ok := c.Load("fake", "h"); ok {
		t.Fatal("cache returned entry with stale MochiVersion")
	}
}

// TestCacheInvalidationOnVersionByteCorruption confirms a tampered
// version byte is refused.
func TestCacheInvalidationOnVersionByteCorruption(t *testing.T) {
	dir := t.TempDir()
	c := &Cache{Dir: dir}
	pb := &PackageBinding{
		ImportPath:   "fake",
		Name:         "fake",
		GoSumHash:    "h",
		MochiVersion: MochiResolverVersion,
	}
	if err := c.Store(pb); err != nil {
		t.Fatal(err)
	}
	c.memo = nil
	// Corrupt the version byte on disk.
	path := c.path("fake", "h")
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	data[0] = 0xFF
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatal(err)
	}
	if _, ok := c.Load("fake", "h"); ok {
		t.Fatal("cache returned entry with bad version byte")
	}
}

// TestCacheInvalidate drops an entry and confirms the file is gone.
func TestCacheInvalidate(t *testing.T) {
	dir := t.TempDir()
	c := &Cache{Dir: dir}
	r := &Resolver{Cache: c, GoSumHash: "h"}
	if _, err := r.Resolve("strings"); err != nil {
		t.Fatal(err)
	}
	if err := c.Invalidate("strings"); err != nil {
		t.Fatal(err)
	}
	c.memo = nil
	if _, ok := c.Load("strings", "h"); ok {
		t.Fatal("cache still hit after invalidate")
	}
	entries, _ := os.ReadDir(dir)
	for _, e := range entries {
		if filepath.Ext(e.Name()) == ".bin" {
			t.Errorf("file still present after invalidate: %s", e.Name())
		}
	}
}

// TestDefaultCacheDir checks the platform fallbacks.
func TestDefaultCacheDir(t *testing.T) {
	if DefaultCacheDir() == "" {
		t.Fatal("DefaultCacheDir returned empty string")
	}
	// XDG override is honoured.
	t.Setenv("XDG_CACHE_HOME", "/tmp/xdg-test")
	got := DefaultCacheDir()
	if got != "/tmp/xdg-test/mochi/bindings" {
		t.Errorf("XDG-derived dir = %q", got)
	}
}

// TestHashGoSumEmptyPath confirms the empty-path / missing-file
// degrades to ("", nil) so callers outside a module just see "no
// module context" not an error.
func TestHashGoSumEmptyPath(t *testing.T) {
	h, err := HashGoSum("")
	if err != nil || h != "" {
		t.Fatalf("empty path: h=%q err=%v", h, err)
	}
	h, err = HashGoSum("/this/file/does/not/exist")
	if err != nil || h != "" {
		t.Fatalf("missing file: h=%q err=%v", h, err)
	}
}

// TestHashGoSumRealFile feeds a real go.sum into the hasher.
func TestHashGoSumRealFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "go.sum")
	if err := os.WriteFile(path, []byte("example/mod v1.0.0/go.mod h1:abc\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	h, err := HashGoSum(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(h) != 64 {
		t.Errorf("hash length = %d, want 64", len(h))
	}
	// Hash is deterministic on content.
	h2, _ := HashGoSum(path)
	if h != h2 {
		t.Error("hash not deterministic")
	}
}
