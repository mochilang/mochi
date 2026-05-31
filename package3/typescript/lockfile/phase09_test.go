package lockfile_test

import (
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/typescript/lockfile"
)

// TestPhase9Lockfile is the MEP-72 Phase 9 gate for the lockfile package.
func TestPhase9Lockfile(t *testing.T) {
	f := &lockfile.File{}
	f.AddNpm(lockfile.NpmEntry{
		Name:    "lodash",
		Version: "4.17.21",
		SHA512:  "sha512-v2kDEe57lecTulaDIuNTPy3Ry4gL==",
	})
	f.AddJsr(lockfile.JsrEntry{
		Scope:   "std",
		Package: "fs",
		Version: "1.0.0",
		SHA256:  "abc123",
	})

	outDir := t.TempDir()
	lockPath := filepath.Join(outDir, "mochi.lock")
	if err := lockfile.Write(lockPath, f); err != nil {
		t.Fatalf("Write: %v", err)
	}

	// Re-read.
	got, err := lockfile.Read(lockPath)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if len(got.NpmPackages) != 1 {
		t.Errorf("NpmPackages count = %d, want 1", len(got.NpmPackages))
	} else {
		np := got.NpmPackages[0]
		if np.Name != "lodash" || np.Version != "4.17.21" {
			t.Errorf("npm entry = %+v", np)
		}
		if !strings.HasPrefix(np.SHA512, "sha512-") {
			t.Errorf("sha512 = %q, want sha512- prefix", np.SHA512)
		}
	}
	if len(got.JsrPackages) != 1 {
		t.Errorf("JsrPackages count = %d, want 1", len(got.JsrPackages))
	} else {
		jp := got.JsrPackages[0]
		if jp.Scope != "std" || jp.Package != "fs" || jp.Version != "1.0.0" {
			t.Errorf("jsr entry = %+v", jp)
		}
	}

	// AddNpm idempotent (update).
	f.AddNpm(lockfile.NpmEntry{Name: "lodash", Version: "4.17.21", SHA512: "sha512-updated=="})
	if len(f.NpmPackages) != 1 {
		t.Errorf("AddNpm should update, not append; count = %d", len(f.NpmPackages))
	}
}
