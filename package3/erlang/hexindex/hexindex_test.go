package hexindex

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"

	"mochi/package3/erlang/hexsemver"
)

// buildMockPackageMeta returns a JSON-encoded PackageMeta for use in tests.
func buildMockPackageMeta(name string, versions []string) []byte {
	releases := make([]Release, len(versions))
	for i, v := range versions {
		releases[i] = Release{
			Version:       v,
			OuterChecksum: "aa" + v, // fake checksum for test
			Requirements:  map[string]ReleaseRequirement{},
		}
	}
	meta := PackageMeta{Name: name, Releases: releases}
	b, _ := json.Marshal(meta)
	return b
}

// buildMockTarball creates a minimal Hex.pm-style outer .tar containing
// contents.tar.gz (which is itself a gzip archive of a single file).
func buildMockTarball(innerContent []byte) []byte {
	// Build inner tar.gz
	var innerBuf bytes.Buffer
	gw := gzip.NewWriter(&innerBuf)
	tw := tar.NewWriter(gw)
	_ = tw.WriteHeader(&tar.Header{Name: "src/lib.erl", Size: int64(len(innerContent)), Mode: 0o644})
	_, _ = tw.Write(innerContent)
	tw.Close()
	gw.Close()
	innerGZ := innerBuf.Bytes()

	// Build outer tar (bare, no compression) with two members:
	//   metadata.config + contents.tar.gz
	var outerBuf bytes.Buffer
	otw := tar.NewWriter(&outerBuf)
	meta := []byte("{vsn, 3}.\n")
	_ = otw.WriteHeader(&tar.Header{Name: "metadata.config", Size: int64(len(meta)), Mode: 0o644})
	_, _ = otw.Write(meta)
	_ = otw.WriteHeader(&tar.Header{Name: "contents.tar.gz", Size: int64(len(innerGZ)), Mode: 0o644})
	_, _ = otw.Write(innerGZ)
	otw.Close()
	return outerBuf.Bytes()
}

func TestGetPackage_Success(t *testing.T) {
	body := buildMockPackageMeta("cowboy", []string{"2.12.0", "2.11.0", "2.10.0"})
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/packages/cowboy" {
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		w.Write(body)
	}))
	defer srv.Close()

	client := NewClient(t.TempDir())
	client.BaseURL = srv.URL
	client.HTTPClient = srv.Client()

	meta, err := client.GetPackage(context.Background(), "cowboy")
	if err != nil {
		t.Fatalf("GetPackage: %v", err)
	}
	if meta.Name != "cowboy" {
		t.Errorf("Name = %q, want cowboy", meta.Name)
	}
	if len(meta.Releases) != 3 {
		t.Errorf("len(Releases) = %d, want 3", len(meta.Releases))
	}
}

func TestGetPackage_NotFound(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	}))
	defer srv.Close()

	client := NewClient(t.TempDir())
	client.BaseURL = srv.URL
	client.HTTPClient = srv.Client()

	_, err := client.GetPackage(context.Background(), "nonexistent")
	if err == nil {
		t.Error("GetPackage should fail for 404")
	}
}

func TestActiveVersions_FiltersRetired(t *testing.T) {
	retired := time.Now()
	meta := &PackageMeta{
		Name: "ranch",
		Releases: []Release{
			{Version: "2.1.0"},
			{Version: "1.8.0", RetiredAt: &retired},
			{Version: "2.0.0"},
		},
	}
	active := ActiveVersions(meta)
	if len(active) != 2 {
		t.Errorf("len(active) = %d, want 2 (should filter retired)", len(active))
	}
	for _, v := range active {
		if v.String() == "1.8.0" {
			t.Error("retired version 1.8.0 should be filtered out")
		}
	}
}

func TestResolveVersion_TildeGreater(t *testing.T) {
	meta := &PackageMeta{
		Name: "hackney",
		Releases: []Release{
			{Version: "1.20.1"},
			{Version: "1.20.0"},
			{Version: "1.19.3"},
			{Version: "2.0.0"},
		},
	}
	v, err := ResolveVersion(meta, "~> 1.20")
	if err != nil {
		t.Fatalf("ResolveVersion: %v", err)
	}
	if v.String() != "1.20.1" {
		t.Errorf("resolved = %q, want 1.20.1", v.String())
	}
}

func TestResolveVersion_NoMatch(t *testing.T) {
	meta := &PackageMeta{
		Name:     "jsx",
		Releases: []Release{{Version: "2.9.0"}, {Version: "2.8.0"}},
	}
	_, err := ResolveVersion(meta, "~> 3.0")
	if err == nil {
		t.Error("ResolveVersion should fail when no version satisfies constraint")
	}
}

func TestResolveVersion_ExactMatch(t *testing.T) {
	meta := &PackageMeta{
		Name:     "poolboy",
		Releases: []Release{{Version: "1.5.2"}, {Version: "1.5.1"}},
	}
	v, err := ResolveVersion(meta, "== 1.5.1")
	if err != nil {
		t.Fatalf("ResolveVersion exact: %v", err)
	}
	if v.String() != "1.5.1" {
		t.Errorf("resolved = %q, want 1.5.1", v.String())
	}
}

func TestDownloadTarball_VerifyAndCache(t *testing.T) {
	innerContent := []byte("-module(cowboy).\n")
	outerTar := buildMockTarball(innerContent)

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/tarballs/cowboy-2.12.0.tar" {
			w.Write(outerTar)
			return
		}
		http.NotFound(w, r)
	}))
	defer srv.Close()

	cache := t.TempDir()
	client := NewClient(cache)
	client.BaseURL = srv.URL
	client.HTTPClient = srv.Client()

	res, err := client.DownloadTarball(context.Background(), "cowboy", "2.12.0")
	if err != nil {
		t.Fatalf("DownloadTarball: %v", err)
	}

	// Outer SHA-256 should match.
	h := sha256.Sum256(outerTar)
	wantOuter := hex.EncodeToString(h[:])
	if res.OuterSHA256 != wantOuter {
		t.Errorf("OuterSHA256 = %q, want %q", res.OuterSHA256, wantOuter)
	}
	// InnerSHA256 and InnerSHA512 should be non-empty.
	if res.InnerSHA256 == "" || res.InnerSHA512 == "" {
		t.Error("InnerSHA256/InnerSHA512 should be populated")
	}
	// Files should exist on disk.
	if _, err := os.Stat(res.OuterPath); os.IsNotExist(err) {
		t.Errorf("outer.tar not cached at %s", res.OuterPath)
	}
	if _, err := os.Stat(res.InnerPath); os.IsNotExist(err) {
		t.Errorf("inner.tar.gz not cached at %s", res.InnerPath)
	}
}

func TestDownloadTarball_CacheHit(t *testing.T) {
	innerContent := []byte("-module(ranch).\n")
	outerTar := buildMockTarball(innerContent)

	callCount := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		callCount++
		w.Write(outerTar)
	}))
	defer srv.Close()

	cache := t.TempDir()
	client := NewClient(cache)
	client.BaseURL = srv.URL
	client.HTTPClient = srv.Client()

	// First call: downloads.
	_, err := client.DownloadTarball(context.Background(), "ranch", "2.1.0")
	if err != nil {
		t.Fatalf("first download: %v", err)
	}
	// Second call: should use cache.
	_, err = client.DownloadTarball(context.Background(), "ranch", "2.1.0")
	if err != nil {
		t.Fatalf("second download: %v", err)
	}
	if callCount != 1 {
		t.Errorf("HTTP called %d times, want 1 (cache should prevent second call)", callCount)
	}
}

func TestDownloadTarball_NoCache(t *testing.T) {
	innerContent := []byte("-module(hackney).\n")
	outerTar := buildMockTarball(innerContent)

	callCount := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		callCount++
		w.Write(outerTar)
	}))
	defer srv.Close()

	cache := t.TempDir()
	client := NewClient(cache)
	client.BaseURL = srv.URL
	client.HTTPClient = srv.Client()
	client.NoCache = true

	client.DownloadTarball(context.Background(), "hackney", "1.20.1")
	client.DownloadTarball(context.Background(), "hackney", "1.20.1")
	if callCount != 2 {
		t.Errorf("HTTP called %d times with NoCache=true, want 2", callCount)
	}
}

func TestVerifyChecksum_Match(t *testing.T) {
	cache := t.TempDir()
	data := []byte("hello world")
	h := sha256.Sum256(data)
	wantHex := hex.EncodeToString(h[:])

	outerPath := filepath.Join(cache, "outer.tar")
	innerPath := filepath.Join(cache, "inner.tar.gz")
	os.WriteFile(outerPath, data, 0o644)
	os.WriteFile(innerPath, data, 0o644)

	res := &DownloadResult{OuterPath: outerPath, InnerPath: innerPath}
	if err := VerifyChecksum(res, wantHex, wantHex); err != nil {
		t.Errorf("VerifyChecksum: %v", err)
	}
}

func TestVerifyChecksum_Mismatch(t *testing.T) {
	cache := t.TempDir()
	outerPath := filepath.Join(cache, "outer.tar")
	os.WriteFile(outerPath, []byte("real data"), 0o644)

	res := &DownloadResult{OuterPath: outerPath}
	if err := VerifyChecksum(res, "000000", ""); err == nil {
		t.Error("VerifyChecksum should fail on SHA-256 mismatch")
	}
}

func TestExtractInner_MissingFile(t *testing.T) {
	// An outer tar with no contents.tar.gz member.
	var buf bytes.Buffer
	tw := tar.NewWriter(&buf)
	tw.WriteHeader(&tar.Header{Name: "metadata.config", Size: 4, Mode: 0o644})
	tw.Write([]byte("data"))
	tw.Close()

	_, err := extractInner(buf.Bytes())
	if err == nil {
		t.Error("extractInner should fail when contents.tar.gz is absent")
	}
}

func TestActiveVersions_InvalidVersion_Skipped(t *testing.T) {
	meta := &PackageMeta{
		Name:     "test",
		Releases: []Release{{Version: "notaversion"}, {Version: "1.0.0"}},
	}
	vs := ActiveVersions(meta)
	if len(vs) != 1 {
		t.Errorf("len = %d, want 1 (invalid version skipped)", len(vs))
	}
	if vs[0] != (hexsemver.Version{Major: 1, Minor: 0, Patch: 0}) {
		t.Errorf("unexpected version: %+v", vs[0])
	}
}
