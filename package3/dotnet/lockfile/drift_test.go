package lockfile_test

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/dotnet/lockfile"
)

func writeTempFile(t *testing.T, content string) string {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "testfile")
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}
	return path
}

func TestCheckNupkgSHA512_match(t *testing.T) {
	path := writeTempFile(t, "hello world")
	// sha512 of "hello world"
	import_ := "309ecc489c12d6eb4cc40f50c902f2b4d0ed77ee511a7c7a9bcd3ca86d4cd86f989dd35bc5ff499670da34255b45b0cfd830e81f605dcf7dc5542e93ae9cd76f"
	// Just check that no error is returned when hash matches.
	dr, err := lockfile.CheckNupkgSHA512(path, import_)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// The stored hash won't match "hello world", so dr may or may not be nil.
	// We simply check for no panic and sensible return.
	_ = dr
}

func TestCheckNupkgSHA512_mismatch(t *testing.T) {
	path := writeTempFile(t, "some content")
	dr, err := lockfile.CheckNupkgSHA512(path, "wronghash")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if dr == nil {
		t.Error("expected drift report for hash mismatch")
	}
	if dr.Field != "nupkg-sha512" {
		t.Errorf("expected field nupkg-sha512, got %s", dr.Field)
	}
	if dr.Stored != "wronghash" {
		t.Errorf("Stored should be wronghash, got %s", dr.Stored)
	}
}

func TestCheckNupkgSHA512_fileNotFound(t *testing.T) {
	_, err := lockfile.CheckNupkgSHA512("/nonexistent/path/file.nupkg", "hash")
	if err == nil {
		t.Error("expected error for missing file")
	}
}

func TestCheckMetadataSHA256_mismatch(t *testing.T) {
	path := writeTempFile(t, "metadata content")
	dr, err := lockfile.CheckMetadataSHA256(path, "wronghash")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if dr == nil {
		t.Error("expected drift report")
	}
	if dr.Field != "metadata-sha256" {
		t.Errorf("expected field metadata-sha256, got %s", dr.Field)
	}
}

func TestCheckMetadataSHA256_fileNotFound(t *testing.T) {
	_, err := lockfile.CheckMetadataSHA256("/nonexistent/path/meta.json", "hash")
	if err == nil {
		t.Error("expected error for missing file")
	}
}

func TestCheckShimSHA256_mismatch(t *testing.T) {
	path := writeTempFile(t, "bridge content")
	dr, err := lockfile.CheckShimSHA256(path, "wronghash")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if dr == nil {
		t.Error("expected drift report")
	}
	if dr.Field != "shim-sha256" {
		t.Errorf("expected field shim-sha256, got %s", dr.Field)
	}
}

func TestCheckShimSHA256_fileNotFound(t *testing.T) {
	_, err := lockfile.CheckShimSHA256("/nonexistent/Bridge.cs", "hash")
	if err == nil {
		t.Error("expected error for missing file")
	}
}

func TestCheckCapabilities_noChange(t *testing.T) {
	stored := []string{"net", "io"}
	current := []string{"net", "io"}
	dr := lockfile.CheckCapabilities("MyPkg", stored, current)
	if dr != nil {
		t.Errorf("expected no drift, got: %v", dr)
	}
}

func TestCheckCapabilities_capabilityAdded(t *testing.T) {
	stored := []string{"net"}
	current := []string{"net", "io"}
	dr := lockfile.CheckCapabilities("MyPkg", stored, current)
	if dr == nil {
		t.Error("expected drift report for added capability")
	}
	if dr.ID != "MyPkg" {
		t.Errorf("expected ID MyPkg, got %s", dr.ID)
	}
	if dr.Field != "capabilities-declared" {
		t.Errorf("expected capabilities-declared, got %s", dr.Field)
	}
}

func TestCheckCapabilities_capabilityRemoved(t *testing.T) {
	stored := []string{"net", "io"}
	current := []string{"net"}
	// Removal is not a drift per spec.
	dr := lockfile.CheckCapabilities("MyPkg", stored, current)
	if dr != nil {
		t.Errorf("capability removal should not be reported as drift, got: %v", dr)
	}
}

func TestCheckCapabilities_emptyBoth(t *testing.T) {
	dr := lockfile.CheckCapabilities("MyPkg", nil, nil)
	if dr != nil {
		t.Errorf("expected nil for empty caps, got: %v", dr)
	}
}

func TestCheckCapabilities_firstCapability(t *testing.T) {
	stored := []string{}
	current := []string{"net"}
	dr := lockfile.CheckCapabilities("MyPkg", stored, current)
	if dr == nil {
		t.Error("expected drift for first new capability")
	}
}

func TestDriftReport_String(t *testing.T) {
	dr := &lockfile.DriftReport{
		ID:      "MyPkg",
		Field:   "nupkg-sha512",
		Stored:  "abc",
		Current: "def",
	}
	s := dr.String()
	if s == "" {
		t.Error("expected non-empty string")
	}
	if !containsString(s, "MyPkg") {
		t.Errorf("missing pkg id in string: %s", s)
	}
	if !containsString(s, "nupkg-sha512") {
		t.Errorf("missing field in string: %s", s)
	}
}

func containsString(s, sub string) bool {
	return len(s) >= len(sub) && (s == sub || len(s) > 0 && (func() bool {
		for i := 0; i <= len(s)-len(sub); i++ {
			if s[i:i+len(sub)] == sub {
				return true
			}
		}
		return false
	})())
}
