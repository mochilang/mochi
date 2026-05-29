package rbs

import (
	"archive/tar"
	"compress/gzip"
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
)

// ---- test helpers -----------------------------------------------------------

// buildManifestJSON encodes a slice of CollectionEntry into the JSON format
// expected by FetchManifest.
func buildManifestJSON(entries []CollectionEntry) []byte {
	type raw struct {
		Name    string `json:"name"`
		Version string `json:"version"`
		Source  struct {
			Type     string `json:"type"`
			Repo     string `json:"repo"`
			Revision string `json:"revision"`
		} `json:"source"`
	}
	var raws []raw
	for _, e := range entries {
		r := raw{Name: e.Name, Version: e.Version}
		r.Source.Type = e.Source.Type
		r.Source.Repo = e.Source.Repo
		r.Source.Revision = e.Source.Revision
		raws = append(raws, r)
	}
	data, _ := json.Marshal(raws)
	return data
}

// buildTarGZ creates an in-memory tar.gz with the given files.
// Each entry in files maps a path inside the archive to its content.
func buildTarGZ(files map[string]string) []byte {
	var buf bytes.Buffer
	gz := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gz)
	for name, content := range files {
		hdr := &tar.Header{
			Name:     name,
			Typeflag: tar.TypeReg,
			Size:     int64(len(content)),
			Mode:     0o644,
		}
		_ = tw.WriteHeader(hdr)
		_, _ = tw.Write([]byte(content))
	}
	_ = tw.Close()
	_ = gz.Close()
	return buf.Bytes()
}

// testEntries is a shared set of collection entries used across tests.
var testEntries = []CollectionEntry{
	{Name: "redis", Version: "4.0", Source: CollectionSource{Type: "github", Repo: "ruby/gem_rbs_collection", Revision: "abc123"}},
	{Name: "redis", Version: "5.0", Source: CollectionSource{Type: "github", Repo: "ruby/gem_rbs_collection", Revision: "def456"}},
	{Name: "nokogiri", Version: "1.0", Source: CollectionSource{Type: "github", Repo: "ruby/gem_rbs_collection", Revision: "abc123"}},
	{Name: "activesupport", Version: "7.0", Source: CollectionSource{Type: "github", Repo: "ruby/gem_rbs_collection", Revision: "abc123"}},
}

// ---- tests ------------------------------------------------------------------

// TestFetchManifestParsesJSON verifies that FetchManifest correctly decodes the
// gems.json returned by the mock server.
func TestFetchManifestParsesJSON(t *testing.T) {
	manifest := buildManifestJSON(testEntries)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(manifest)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL + "/tar.gz/refs/heads/main",
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}
	entries, sha, err := f.FetchManifest(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != len(testEntries) {
		t.Errorf("expected %d entries, got %d", len(testEntries), len(entries))
	}
	if sha == "" {
		t.Error("sha256 should not be empty")
	}
}

// TestFetchManifestEntryFields verifies individual field decoding.
func TestFetchManifestEntryFields(t *testing.T) {
	manifest := buildManifestJSON(testEntries[:1])
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(manifest)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL + "/tar.gz/refs/heads/main",
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}
	entries, _, err := f.FetchManifest(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	e := entries[0]
	if e.Name != "redis" {
		t.Errorf("Name = %q, want %q", e.Name, "redis")
	}
	if e.Version != "4.0" {
		t.Errorf("Version = %q, want %q", e.Version, "4.0")
	}
	if e.Source.Type != "github" {
		t.Errorf("Source.Type = %q, want %q", e.Source.Type, "github")
	}
	if e.Source.Repo != "ruby/gem_rbs_collection" {
		t.Errorf("Source.Repo = %q, want %q", e.Source.Repo, "ruby/gem_rbs_collection")
	}
}

// TestFetchManifestETagCaching verifies that the second request uses the cached copy.
func TestFetchManifestETagCaching(t *testing.T) {
	manifest := buildManifestJSON(testEntries)
	callCount := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		callCount++
		if r.Header.Get("If-None-Match") == `"test-etag"` {
			w.WriteHeader(http.StatusNotModified)
			return
		}
		w.Header().Set("ETag", `"test-etag"`)
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(manifest)
	}))
	defer srv.Close()

	cacheDir := t.TempDir()
	f := &Fetcher{
		BaseURL:  srv.URL + "/tar.gz/refs/heads/main",
		HTTP:     srv.Client(),
		CacheDir: cacheDir,
	}

	// First call: populates cache.
	_, _, err := f.FetchManifest(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	// Second call: should get 304 and use cache.
	entries, _, err := f.FetchManifest(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != len(testEntries) {
		t.Errorf("cached result has %d entries, want %d", len(entries), len(testEntries))
	}
	if callCount != 2 {
		t.Errorf("expected 2 HTTP calls, got %d", callCount)
	}
}

// TestFindEntryExactMatch verifies that FindEntry returns the exact version match.
func TestFindEntryExactMatch(t *testing.T) {
	e := FindEntry(testEntries, "redis", "4.0.5")
	if e == nil {
		t.Fatal("expected a match, got nil")
	}
	if e.Version != "4.0" {
		t.Errorf("Version = %q, want %q", e.Version, "4.0")
	}
}

// TestFindEntryHigherVersion verifies that FindEntry returns the highest version
// when there is no exact match.
func TestFindEntryHigherVersion(t *testing.T) {
	e := FindEntry(testEntries, "redis", "5.1.0")
	if e == nil {
		t.Fatal("expected a match, got nil")
	}
	if e.Version != "5.0" {
		t.Errorf("Version = %q, want %q", e.Version, "5.0")
	}
}

// TestFindEntryNotFound verifies that FindEntry returns nil for unknown gems.
func TestFindEntryNotFound(t *testing.T) {
	e := FindEntry(testEntries, "unknown_gem", "1.0.0")
	if e != nil {
		t.Errorf("expected nil for unknown gem, got %+v", e)
	}
}

// TestFindEntryNokogiri verifies that a singly-versioned gem is found correctly.
func TestFindEntryNokogiri(t *testing.T) {
	e := FindEntry(testEntries, "nokogiri", "1.16.2")
	if e == nil {
		t.Fatal("expected a match for nokogiri, got nil")
	}
	if e.Name != "nokogiri" {
		t.Errorf("Name = %q, want %q", e.Name, "nokogiri")
	}
}

// TestFindEntryEmptyList verifies that FindEntry handles an empty manifest.
func TestFindEntryEmptyList(t *testing.T) {
	e := FindEntry(nil, "redis", "4.0.0")
	if e != nil {
		t.Errorf("expected nil on empty list, got %+v", e)
	}
}

// TestFetchRBSReturnsGemSurface verifies that FetchRBS produces a valid
// GemSurface when the archive contains matching .rbs files.
func TestFetchRBSReturnsGemSurface(t *testing.T) {
	redisRBS := `
class Redis
  def get: (String key) -> String?
  def set: (String key, String value) -> String
end
`
	archiveFiles := map[string]string{
		"gem_rbs_collection-main/gems/redis/4.0/redis.rbs": redisRBS,
	}
	archive := buildTarGZ(archiveFiles)

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/x-gzip")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(archive)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL,
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}

	entries := []CollectionEntry{
		{Name: "redis", Version: "4.0"},
	}
	surface, err := f.FetchRBS(context.Background(), "redis", "4.0.5", entries)
	if err != nil {
		t.Fatal(err)
	}
	if surface == nil {
		t.Fatal("expected a surface, got nil")
	}
	if surface.Source != SourceGemRBSCollection {
		t.Errorf("Source = %v, want SourceGemRBSCollection", surface.Source)
	}
	if surface.Gem != "redis" {
		t.Errorf("Gem = %q, want %q", surface.Gem, "redis")
	}
}

// TestFetchRBSNilForMissingGem verifies that FetchRBS returns nil when the gem
// is not in the collection entries.
func TestFetchRBSNilForMissingGem(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL,
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}
	surface, err := f.FetchRBS(context.Background(), "nonexistent", "1.0.0", testEntries)
	if err != nil {
		t.Fatal(err)
	}
	if surface != nil {
		t.Errorf("expected nil surface for unknown gem, got %+v", surface)
	}
}

// TestFetchRBSSHA256IsSet verifies that the RBSSHA256 field is populated.
func TestFetchRBSSHA256IsSet(t *testing.T) {
	redisRBS := `class Redis; def ping: () -> String; end`
	archive := buildTarGZ(map[string]string{
		"gem_rbs_collection-main/gems/redis/4.0/redis.rbs": redisRBS,
	})
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(archive)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL,
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}
	entries := []CollectionEntry{{Name: "redis", Version: "4.0"}}
	surface, err := f.FetchRBS(context.Background(), "redis", "4.0.0", entries)
	if err != nil {
		t.Fatal(err)
	}
	if surface == nil {
		t.Fatal("expected surface, got nil")
	}
	if surface.RBSSHA256 == "" {
		t.Error("RBSSHA256 should not be empty")
	}
}

// TestFetchRBSParsesClasses verifies that the classes in the RBS files are
// actually parsed and surfaced.
func TestFetchRBSParsesClasses(t *testing.T) {
	rbsContent := `
class Nokogiri::HTML::Document
  def css: (String selector) -> NodeSet
  def at_css: (String selector) -> Node?
end
`
	archive := buildTarGZ(map[string]string{
		"gem_rbs_collection-main/gems/nokogiri/1.0/document.rbs": rbsContent,
	})
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(archive)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL,
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}
	entries := []CollectionEntry{{Name: "nokogiri", Version: "1.0"}}
	surface, err := f.FetchRBS(context.Background(), "nokogiri", "1.16.2", entries)
	if err != nil {
		t.Fatal(err)
	}
	if surface == nil {
		t.Fatal("surface is nil")
	}
	if len(surface.Classes) == 0 {
		t.Fatal("expected at least one class, got zero")
	}
	found := false
	for _, c := range surface.Classes {
		if c.Name == "Nokogiri::HTML::Document" {
			found = true
			break
		}
	}
	if !found {
		t.Error("Nokogiri::HTML::Document not found in parsed surface")
	}
}

// TestNewFetcher verifies that NewFetcher sets expected defaults.
func TestNewFetcher(t *testing.T) {
	f := NewFetcher("/tmp/testcache")
	if f.BaseURL == "" {
		t.Error("BaseURL should not be empty")
	}
	if f.CacheDir != "/tmp/testcache" {
		t.Errorf("CacheDir = %q, want %q", f.CacheDir, "/tmp/testcache")
	}
	if f.HTTP != http.DefaultClient {
		t.Error("HTTP should default to http.DefaultClient")
	}
}

// TestParseManifestJSONEmpty verifies that an empty array is handled.
func TestParseManifestJSONEmpty(t *testing.T) {
	entries, sha, err := parseManifestJSON([]byte("[]"))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 0 {
		t.Errorf("expected 0 entries, got %d", len(entries))
	}
	if sha == "" {
		t.Error("sha256 should not be empty for empty array")
	}
}

// TestFetchManifestHTTPError verifies that FetchManifest returns an error on
// non-200 responses when there is no cache.
func TestFetchManifestHTTPError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
	}))
	defer srv.Close()

	f := &Fetcher{
		BaseURL:  srv.URL + "/tar.gz/refs/heads/main",
		HTTP:     srv.Client(),
		CacheDir: t.TempDir(),
	}
	_, _, err := f.FetchManifest(context.Background())
	if err == nil {
		t.Error("expected error on HTTP 500, got nil")
	}
}

// TestFetchManifestCacheUsedOnNetworkError verifies that the cached manifest is
// returned when the network request fails.
func TestFetchManifestCacheUsedOnNetworkError(t *testing.T) {
	manifest := buildManifestJSON(testEntries)

	// Pre-populate the cache manually.
	cacheDir := t.TempDir()
	base := filepath.Join(cacheDir, "gem_rbs_collection")
	_ = os.MkdirAll(base, 0o755)
	_ = os.WriteFile(filepath.Join(base, "manifest.json"), manifest, 0o644)

	// Point the fetcher at a non-existent server.
	f := &Fetcher{
		BaseURL:  "http://127.0.0.1:0/tar.gz/refs/heads/main",
		HTTP:     &http.Client{},
		CacheDir: cacheDir,
	}
	entries, _, err := f.FetchManifest(context.Background())
	if err != nil {
		t.Fatalf("expected cache fallback, got error: %v", err)
	}
	if len(entries) != len(testEntries) {
		t.Errorf("expected %d cached entries, got %d", len(testEntries), len(entries))
	}
}

// TestToMajorMinor verifies the version normalisation helper.
func TestToMajorMinor(t *testing.T) {
	cases := []struct{ in, want string }{
		{"4.8.3", "4.8"},
		{"1.0", "1.0"},
		{"10", "10"},
		{"5.0.0.beta", "5.0"},
	}
	for _, c := range cases {
		t.Run(c.in, func(t *testing.T) {
			got := toMajorMinor(c.in)
			if got != c.want {
				t.Errorf("toMajorMinor(%q) = %q, want %q", c.in, got, c.want)
			}
		})
	}
}
