package lock_test

import (
	"path/filepath"
	"testing"

	"mochi/package3/swift/lock"
)

// TestPhase8LockGate is the phase 8 gate test. It verifies the round-trip
// integrity of the lock file (write then read returns identical data) and that
// drift detection runs without error.
func TestPhase8LockGate(t *testing.T) {
	dir := t.TempDir()
	lockPath := filepath.Join(dir, "phase08.lock")

	entries := []lock.Entry{
		{
			URL:       "https://github.com/apple/swift-argument-parser.git",
			Version:   "1.3.0",
			SHA256:    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
			Platforms: []string{"macos-arm64", "macos-x86_64"},
		},
		{
			URL:       "https://github.com/apple/swift-collections.git",
			Version:   "1.1.0",
			SHA256:    "d2d6d0d09b785b2cc9153e2a69029898b8a92bbc1c7c49c6d59a05d3e5b7e5a2",
			Platforms: []string{"macos-arm64"},
		},
	}

	// Write
	if err := lock.Write(lockPath, entries); err != nil {
		t.Fatalf("Phase8 Write: %v", err)
	}

	// Read back
	got, err := lock.Read(lockPath)
	if err != nil {
		t.Fatalf("Phase8 Read: %v", err)
	}

	if len(got) != len(entries) {
		t.Fatalf("Phase8 round-trip: got %d entries, want %d", len(got), len(entries))
	}

	// Build map for comparison
	byURL := map[string]lock.Entry{}
	for _, e := range got {
		byURL[e.URL] = e
	}

	for _, want := range entries {
		got, ok := byURL[want.URL]
		if !ok {
			t.Errorf("Phase8: entry %s missing after round-trip", want.URL)
			continue
		}
		if got.Version != want.Version {
			t.Errorf("Phase8: %s Version = %q, want %q", want.URL, got.Version, want.Version)
		}
		if got.SHA256 != want.SHA256 {
			t.Errorf("Phase8: %s SHA256 = %q, want %q", want.URL, got.SHA256, want.SHA256)
		}
		if len(got.Platforms) != len(want.Platforms) {
			t.Errorf("Phase8: %s Platforms = %v, want %v", want.URL, got.Platforms, want.Platforms)
		}
	}

	// Drift check with empty cache (no files present = no drift reported)
	reports, err := lock.Check(lockPath, t.TempDir())
	if err != nil {
		t.Fatalf("Phase8 Check: %v", err)
	}
	if len(reports) != 0 {
		t.Errorf("Phase8 drift: got %d reports for empty cache, want 0", len(reports))
	}
}
