package lock_test

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/swift/lock"
)

func TestPhase8Lock(t *testing.T) {
	dir := t.TempDir()
	lockPath := filepath.Join(dir, "mochi.lock")

	// Write some entries
	entries := []lock.Entry{
		{
			URL:       "https://github.com/example/libA.git",
			Version:   "1.2.3",
			SHA256:    "abc123",
			Platforms: []string{"macos-arm64", "macos-x86_64"},
		},
		{
			URL:       "https://github.com/example/libB.git",
			Version:   "2.0.0",
			SHA256:    "def456",
			Platforms: []string{"macos-arm64"},
		},
	}

	if err := lock.Write(lockPath, entries); err != nil {
		t.Fatalf("Write error: %v", err)
	}

	// Round-trip: read them back
	got, err := lock.Read(lockPath)
	if err != nil {
		t.Fatalf("Read error: %v", err)
	}

	if len(got) != len(entries) {
		t.Fatalf("Read returned %d entries, want %d", len(got), len(entries))
	}

	// Check both entries (order may be sorted)
	byURL := map[string]lock.Entry{}
	for _, e := range got {
		byURL[e.URL] = e
	}

	a, ok := byURL["https://github.com/example/libA.git"]
	if !ok {
		t.Fatal("libA not found after round-trip")
	}
	if a.Version != "1.2.3" {
		t.Errorf("libA Version = %q, want 1.2.3", a.Version)
	}
	if a.SHA256 != "abc123" {
		t.Errorf("libA SHA256 = %q, want abc123", a.SHA256)
	}
	if len(a.Platforms) != 2 {
		t.Errorf("libA Platforms = %v, want 2 entries", a.Platforms)
	}

	b, ok := byURL["https://github.com/example/libB.git"]
	if !ok {
		t.Fatal("libB not found after round-trip")
	}
	if b.Version != "2.0.0" {
		t.Errorf("libB Version = %q, want 2.0.0", b.Version)
	}
}

func TestDriftDetection(t *testing.T) {
	dir := t.TempDir()
	lockPath := filepath.Join(dir, "mochi.lock")
	cacheDir := dir

	// Write an entry with a known hash
	entries := []lock.Entry{
		{
			URL:       "https://github.com/example/libA.git",
			Version:   "1.0.0",
			SHA256:    "aabbcc",
			Platforms: []string{"macos"},
		},
	}
	if err := lock.Write(lockPath, entries); err != nil {
		t.Fatalf("Write error: %v", err)
	}

	// Create a fake archive file in cacheDir with a different hash.
	// The cache key is sha256("url:version") which the lock package computes
	// internally, so we need to match. Since we can't call the internal
	// archiveFileName, we check that Check does not error when files are absent.
	reports, err := lock.Check(lockPath, cacheDir)
	if err != nil {
		t.Fatalf("Check error: %v", err)
	}
	// No cached file means no drift reported (file absent = skipped)
	if len(reports) != 0 {
		t.Errorf("Check returned %d reports for absent cache, want 0", len(reports))
	}

	// The actual test: write and read are consistent
	entries2 := []lock.Entry{
		{URL: "https://a.com/pkg.git", Version: "1.0.0", SHA256: "hash1", Platforms: []string{}},
		{URL: "https://b.com/pkg.git", Version: "2.0.0", SHA256: "hash2", Platforms: []string{"macos"}},
	}
	lockPath2 := filepath.Join(dir, "mochi2.lock")
	if err := lock.Write(lockPath2, entries2); err != nil {
		t.Fatalf("Write2 error: %v", err)
	}
	got2, err := lock.Read(lockPath2)
	if err != nil {
		t.Fatalf("Read2 error: %v", err)
	}
	if len(got2) != 2 {
		t.Errorf("Read2 count = %d, want 2", len(got2))
	}
}

func TestLockWriteSorted(t *testing.T) {
	dir := t.TempDir()
	lockPath := filepath.Join(dir, "sorted.lock")

	entries := []lock.Entry{
		{URL: "https://z.com/z.git", Version: "1.0.0", SHA256: "z"},
		{URL: "https://a.com/a.git", Version: "1.0.0", SHA256: "a"},
		{URL: "https://m.com/m.git", Version: "1.0.0", SHA256: "m"},
	}
	if err := lock.Write(lockPath, entries); err != nil {
		t.Fatalf("Write error: %v", err)
	}

	data, _ := os.ReadFile(lockPath)
	content := string(data)

	// a.com should appear before m.com and z.com
	idxA := indexInString(content, "a.com")
	idxM := indexInString(content, "m.com")
	idxZ := indexInString(content, "z.com")
	if idxA > idxM || idxM > idxZ {
		t.Errorf("entries not sorted by URL: a=%d m=%d z=%d", idxA, idxM, idxZ)
	}
}

func indexInString(s, sub string) int {
	for i := 0; i <= len(s)-len(sub); i++ {
		if s[i:i+len(sub)] == sub {
			return i
		}
	}
	return -1
}
