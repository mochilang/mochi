package lock

import (
	"strings"
	"testing"
)

func TestCheckAllMatch(t *testing.T) {
	entries := []PhpPackage{
		{Name: "guzzlehttp/guzzle", DistSHA256: "abc123", ReflectionSHA256: "def456"},
	}
	hashes := map[string]OnDiskHashes{
		"guzzlehttp/guzzle": {DistSHA256: "abc123", ReflectionSHA256: "def456"},
	}
	ok, drifted := Check(entries, hashes)
	if !ok {
		t.Errorf("expected ok=true; got drifted: %v", drifted)
	}
	if len(drifted) != 0 {
		t.Errorf("expected 0 drifted; got %d", len(drifted))
	}
}

func TestCheckDistDrift(t *testing.T) {
	entries := []PhpPackage{
		{Name: "guzzlehttp/guzzle", DistSHA256: "locked123"},
	}
	hashes := map[string]OnDiskHashes{
		"guzzlehttp/guzzle": {DistSHA256: "ondisk456"},
	}
	ok, drifted := Check(entries, hashes)
	if ok {
		t.Error("expected ok=false for drifted dist-sha256")
	}
	if len(drifted) != 1 {
		t.Fatalf("expected 1 drift entry; got %d", len(drifted))
	}
	if drifted[0].Field != "dist-sha256" {
		t.Errorf("Field = %q; want dist-sha256", drifted[0].Field)
	}
	if drifted[0].Locked != "locked123" {
		t.Errorf("Locked = %q; want locked123", drifted[0].Locked)
	}
	if drifted[0].OnDisk != "ondisk456" {
		t.Errorf("OnDisk = %q; want ondisk456", drifted[0].OnDisk)
	}
}

func TestCheckReflectionDrift(t *testing.T) {
	entries := []PhpPackage{
		{Name: "monolog/monolog", DistSHA256: "same", ReflectionSHA256: "old-reflect"},
	}
	hashes := map[string]OnDiskHashes{
		"monolog/monolog": {DistSHA256: "same", ReflectionSHA256: "new-reflect"},
	}
	ok, drifted := Check(entries, hashes)
	if ok {
		t.Error("expected ok=false for drifted reflection-sha256")
	}
	if len(drifted) != 1 || drifted[0].Field != "reflection-sha256" {
		t.Errorf("expected reflection-sha256 drift; got %v", drifted)
	}
}

func TestCheckNotCached(t *testing.T) {
	entries := []PhpPackage{
		{Name: "psr/log", DistSHA256: "abc"},
	}
	hashes := map[string]OnDiskHashes{} // empty -- not cached
	ok, drifted := Check(entries, hashes)
	if ok {
		t.Error("expected ok=false for not-cached package")
	}
	if len(drifted) != 1 {
		t.Fatalf("expected 1 drift entry; got %d", len(drifted))
	}
	if !strings.Contains(drifted[0].OnDisk, "not cached") {
		t.Errorf("OnDisk should say not cached; got %q", drifted[0].OnDisk)
	}
}

func TestCheckEmptyHashIgnored(t *testing.T) {
	// If the locked hash is empty (not set), drift should not be reported.
	entries := []PhpPackage{
		{Name: "acme/lib", DistSHA256: "", ReflectionSHA256: ""},
	}
	hashes := map[string]OnDiskHashes{
		"acme/lib": {DistSHA256: "anything", ReflectionSHA256: "whatever"},
	}
	ok, drifted := Check(entries, hashes)
	if !ok {
		t.Errorf("empty locked hashes should not trigger drift; got %v", drifted)
	}
}

func TestCheckMultiplePackages(t *testing.T) {
	entries := []PhpPackage{
		{Name: "pkg/a", DistSHA256: "aaa"},
		{Name: "pkg/b", DistSHA256: "bbb"},
		{Name: "pkg/c", DistSHA256: "ccc"},
	}
	hashes := map[string]OnDiskHashes{
		"pkg/a": {DistSHA256: "aaa"},
		"pkg/b": {DistSHA256: "WRONG"},
		"pkg/c": {DistSHA256: "ccc"},
	}
	ok, drifted := Check(entries, hashes)
	if ok {
		t.Error("expected ok=false")
	}
	if len(drifted) != 1 || drifted[0].Name != "pkg/b" {
		t.Errorf("expected only pkg/b drifted; got %v", drifted)
	}
}

func TestFormatBasic(t *testing.T) {
	entries := []PhpPackage{
		{
			Name:             "guzzlehttp/guzzle",
			Version:          "7.8.1",
			DistSHA256:       "abc123",
			ReflectionSHA256: "def456",
		},
	}
	out := Format(entries)
	if !strings.Contains(out, "[[php-package]]") {
		t.Errorf("expected [[php-package]] header; got:\n%s", out)
	}
	if !strings.Contains(out, `"guzzlehttp/guzzle"`) {
		t.Errorf("expected package name; got:\n%s", out)
	}
	if !strings.Contains(out, `"7.8.1"`) {
		t.Errorf("expected version; got:\n%s", out)
	}
	if !strings.Contains(out, `"abc123"`) {
		t.Errorf("expected dist-sha256; got:\n%s", out)
	}
}

func TestFormatMultiple(t *testing.T) {
	entries := []PhpPackage{
		{Name: "pkg/a", Version: "1.0.0"},
		{Name: "pkg/b", Version: "2.0.0"},
	}
	out := Format(entries)
	count := strings.Count(out, "[[php-package]]")
	if count != 2 {
		t.Errorf("expected 2 [[php-package]] blocks; got %d", count)
	}
}

func TestVendorPkg(t *testing.T) {
	cases := []struct {
		name, wantVendor, wantPkg string
	}{
		{"guzzlehttp/guzzle", "guzzlehttp", "guzzle"},
		{"psr/log", "psr", "log"},
		{"nodash", "nodash", "nodash"},
	}
	for _, tc := range cases {
		if v := Vendor(tc.name); v != tc.wantVendor {
			t.Errorf("Vendor(%q) = %q; want %q", tc.name, v, tc.wantVendor)
		}
		if p := Pkg(tc.name); p != tc.wantPkg {
			t.Errorf("Pkg(%q) = %q; want %q", tc.name, p, tc.wantPkg)
		}
	}
}

func TestDriftEntryString(t *testing.T) {
	d := DriftEntry{
		Name:   "guzzlehttp/guzzle",
		Field:  "dist-sha256",
		Locked: "abcdef1234567890",
		OnDisk: "fedcba0987654321",
	}
	s := d.String()
	if !strings.Contains(s, "guzzlehttp/guzzle") {
		t.Errorf("expected package name in string; got %q", s)
	}
	if !strings.Contains(s, "dist-sha256") {
		t.Errorf("expected field name in string; got %q", s)
	}
}
