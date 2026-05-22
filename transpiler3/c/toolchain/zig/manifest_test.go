package zig

import (
	"strings"
	"testing"
)

// TestManifestVersionAlignment asserts every Manifest URL embeds
// the pinned Version. Catches the common pitfall of bumping Version
// but forgetting to update one of the asset URLs.
func TestManifestVersionAlignment(t *testing.T) {
	for key, a := range Manifest {
		if !strings.Contains(a.URL, "/"+Version+"/") || !strings.Contains(a.URL, "-"+Version+".") {
			t.Errorf("%s asset URL %q does not embed Version %q", key, a.URL, Version)
		}
		if len(a.SHA256) != 64 {
			t.Errorf("%s asset SHA256 must be 64 hex chars, got %d (%q)", key, len(a.SHA256), a.SHA256)
		}
		if a.Size <= 0 {
			t.Errorf("%s asset Size must be > 0, got %d", key, a.Size)
		}
		if a.Kind != "tar.xz" && a.Kind != "zip" {
			t.Errorf("%s asset Kind must be tar.xz|zip, got %q", key, a.Kind)
		}
	}
}

// TestManifestTier1Coverage asserts the six host triples that
// Phase 1.3 promises are present.
func TestManifestTier1Coverage(t *testing.T) {
	want := []string{
		"x86_64-linux", "aarch64-linux",
		"x86_64-macos", "aarch64-macos",
		"x86_64-windows", "aarch64-windows",
	}
	for _, k := range want {
		if _, ok := Manifest[k]; !ok {
			t.Errorf("manifest missing tier-1 host %q", k)
		}
	}
}

// TestManifestKey checks the goos/goarch → manifest-key translation
// covers the six tier-1 hosts and rejects unknown pairs.
func TestManifestKey(t *testing.T) {
	cases := []struct {
		goos, goarch, want string
	}{
		{"linux", "amd64", "x86_64-linux"},
		{"linux", "arm64", "aarch64-linux"},
		{"darwin", "amd64", "x86_64-macos"},
		{"darwin", "arm64", "aarch64-macos"},
		{"windows", "amd64", "x86_64-windows"},
		{"windows", "arm64", "aarch64-windows"},
		{"freebsd", "amd64", ""},
		{"linux", "riscv64", ""},
		{"plan9", "amd64", ""},
	}
	for _, c := range cases {
		got := ManifestKey(c.goos, c.goarch)
		if got != c.want {
			t.Errorf("ManifestKey(%q,%q)=%q, want %q", c.goos, c.goarch, got, c.want)
		}
	}
}
