package lockfile_test

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/dotnet/lockfile"
)

// TestPhase8Lock is the MEP-68 Phase 8 gate for lockfile file operations.
//
// It exercises:
//   - Round-trip: Write + Read returns the same entries.
//   - Drift detection: version mismatch is reported.
//   - Check returns empty drift when versions and hashes match.
func TestPhase8Lock(t *testing.T) {
	t.Run("RoundTrip", func(t *testing.T) {
		dir := t.TempDir()
		lockPath := filepath.Join(dir, "mochi.lock")

		entries := []lockfile.Entry{
			{ID: "Newtonsoft.Json", Version: "13.0.3", RIDs: []string{"linux-x64", "osx-arm64"}, SHA256: "deadbeef"},
			{ID: "System.Text.Json", Version: "8.0.0", RIDs: []string{"linux-x64"}},
		}

		if err := lockfile.Write(lockPath, entries); err != nil {
			t.Fatalf("Write: %v", err)
		}

		got, err := lockfile.Read(lockPath)
		if err != nil {
			t.Fatalf("Read: %v", err)
		}
		if len(got) != len(entries) {
			t.Fatalf("Read returned %d entries, want %d", len(got), len(entries))
		}
		for i, e := range entries {
			if got[i].ID != e.ID {
				t.Errorf("[%d] ID: got %q, want %q", i, got[i].ID, e.ID)
			}
			if got[i].Version != e.Version {
				t.Errorf("[%d] Version: got %q, want %q", i, got[i].Version, e.Version)
			}
		}
	})

	t.Run("DriftDetected", func(t *testing.T) {
		dir := t.TempDir()
		lockPath := filepath.Join(dir, "mochi.lock")
		cacheDir := t.TempDir()

		// Write lock pinning version 13.0.3.
		entries := []lockfile.Entry{
			{ID: "Newtonsoft.Json", Version: "13.0.3", SHA256: ""},
		}
		if err := lockfile.Write(lockPath, entries); err != nil {
			t.Fatalf("Write: %v", err)
		}

		// Install version 13.0.2 in the cache.
		installDir := filepath.Join(cacheDir, "Newtonsoft.Json", "13.0.2")
		if err := os.MkdirAll(installDir, 0o755); err != nil {
			t.Fatalf("mkdir: %v", err)
		}
		if err := os.WriteFile(filepath.Join(installDir, "Newtonsoft.Json.nupkg"), []byte("fake"), 0o644); err != nil {
			t.Fatalf("write nupkg: %v", err)
		}

		reports, err := lockfile.Check(lockPath, cacheDir)
		if err != nil {
			t.Fatalf("Check: %v", err)
		}
		if len(reports) == 0 {
			t.Error("Check returned no drift reports; expected version mismatch")
		}
		found := false
		for _, r := range reports {
			if r.ID == "Newtonsoft.Json" && r.Field == "version" {
				found = true
				if r.Stored != "13.0.3" {
					t.Errorf("stored version %q, want 13.0.3", r.Stored)
				}
				if r.Current != "13.0.2" {
					t.Errorf("current version %q, want 13.0.2", r.Current)
				}
			}
		}
		if !found {
			t.Error("no version drift report for Newtonsoft.Json")
		}
	})

	t.Run("NoDrift", func(t *testing.T) {
		dir := t.TempDir()
		lockPath := filepath.Join(dir, "mochi.lock")
		cacheDir := t.TempDir()

		// Install version 13.0.3.
		installDir := filepath.Join(cacheDir, "Newtonsoft.Json", "13.0.3")
		if err := os.MkdirAll(installDir, 0o755); err != nil {
			t.Fatalf("mkdir: %v", err)
		}
		nupkgPath := filepath.Join(installDir, "Newtonsoft.Json.nupkg")
		if err := os.WriteFile(nupkgPath, []byte("fake nupkg content"), 0o644); err != nil {
			t.Fatalf("write nupkg: %v", err)
		}

		// Write lock with empty SHA256 so hash comparison is skipped.
		entries := []lockfile.Entry{
			{ID: "Newtonsoft.Json", Version: "13.0.3", SHA256: ""},
		}
		if err := lockfile.Write(lockPath, entries); err != nil {
			t.Fatalf("Write: %v", err)
		}

		reports, err := lockfile.Check(lockPath, cacheDir)
		if err != nil {
			t.Fatalf("Check: %v", err)
		}
		if len(reports) != 0 {
			t.Errorf("Check returned %d drift reports; expected 0: %v", len(reports), reports)
		}
	})
}
