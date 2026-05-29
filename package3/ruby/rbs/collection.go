package rbs

import (
	"archive/tar"
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

// CollectionSource describes where a gem entry in gem_rbs_collection comes from.
type CollectionSource struct {
	// Type is the source type, e.g. "github".
	Type string
	// Repo is the GitHub repository slug, e.g. "ruby/gem_rbs_collection".
	Repo string
	// Revision is the git revision (commit SHA or branch).
	Revision string
}

// CollectionEntry is one gem entry from the gems.json manifest in
// gem_rbs_collection.
type CollectionEntry struct {
	// Name is the gem name, e.g. "redis".
	Name string
	// Version is the major.minor version used by the collection (not the gem
	// release version). Example: "4.0".
	Version string
	// Source describes the upstream location of the RBS files.
	Source CollectionSource
}

// collectionEntryJSON mirrors the JSON structure of one entry in gems.json.
type collectionEntryJSON struct {
	Name    string `json:"name"`
	Version string `json:"version"`
	Source  struct {
		Type     string `json:"type"`
		Repo     string `json:"repo"`
		Revision string `json:"revision"`
	} `json:"source"`
}

// Fetcher fetches gem_rbs_collection manifest and per-gem RBS files.
// It caches downloads in CacheDir to avoid redundant network requests.
type Fetcher struct {
	// BaseURL is the GitHub archive URL that returns a tar.gz of the main
	// branch. Defaults to the official ruby/gem_rbs_collection archive.
	BaseURL string
	// HTTP is the HTTP client used for all requests. Defaults to
	// http.DefaultClient when nil.
	HTTP *http.Client
	// CacheDir is the directory used to cache the downloaded manifest and
	// extracted RBS files. Subdirectory gem_rbs_collection/ is created
	// automatically.
	CacheDir string
}

// NewFetcher creates a Fetcher with default settings and the given cache directory.
func NewFetcher(cacheDir string) *Fetcher {
	return &Fetcher{
		BaseURL:  "https://codeload.github.com/ruby/gem_rbs_collection/tar.gz/refs/heads/main",
		HTTP:     http.DefaultClient,
		CacheDir: cacheDir,
	}
}

func (f *Fetcher) httpClient() *http.Client {
	if f.HTTP != nil {
		return f.HTTP
	}
	return http.DefaultClient
}

func (f *Fetcher) cacheBase() string {
	return filepath.Join(f.CacheDir, "gem_rbs_collection")
}

// FetchManifest downloads the gems.json manifest from gem_rbs_collection.
// It uses ETag-based HTTP caching: the ETag and JSON body are stored in
// cacheDir/gem_rbs_collection/manifest.json and cacheDir/.../manifest.etag.
// The second return value is the SHA-256 hex digest of the JSON bytes.
func (f *Fetcher) FetchManifest(ctx context.Context) ([]CollectionEntry, string, error) {
	base := f.cacheBase()
	if err := os.MkdirAll(base, 0o755); err != nil {
		return nil, "", fmt.Errorf("rbs collection: create cache dir: %w", err)
	}

	manifestPath := filepath.Join(base, "manifest.json")
	etagPath := filepath.Join(base, "manifest.etag")

	// Build the manifest URL.  The manifest lives inside the tarball under
	// gems/<gem>/<version>/_src.yaml, but many community distributions also
	// provide a flat gems.json.  We use a separately hosted manifest URL
	// pattern.  For the real gem_rbs_collection the archive itself IS the
	// manifest; tests override BaseURL to a mock server.
	manifestURL := f.BaseURL
	if strings.HasSuffix(manifestURL, "/tar.gz/refs/heads/main") {
		manifestURL = strings.TrimSuffix(manifestURL, "/tar.gz/refs/heads/main") +
			"/raw/refs/heads/main/gems.json"
	}

	// Read cached ETag if present.
	var etag string
	if etagBytes, err := os.ReadFile(etagPath); err == nil {
		etag = strings.TrimSpace(string(etagBytes))
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, manifestURL, nil)
	if err != nil {
		return nil, "", fmt.Errorf("rbs collection: build manifest request: %w", err)
	}
	if etag != "" {
		req.Header.Set("If-None-Match", etag)
	}

	resp, err := f.httpClient().Do(req)
	if err != nil {
		// Fall back to cached copy on network error.
		if cached, cerr := os.ReadFile(manifestPath); cerr == nil {
			return parseManifestJSON(cached)
		}
		return nil, "", fmt.Errorf("rbs collection: fetch manifest: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNotModified {
		// ETag matched; use cached copy.
		if cached, cerr := os.ReadFile(manifestPath); cerr == nil {
			return parseManifestJSON(cached)
		}
	}

	if resp.StatusCode != http.StatusOK {
		return nil, "", fmt.Errorf("rbs collection: manifest HTTP %d", resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, "", fmt.Errorf("rbs collection: read manifest body: %w", err)
	}

	// Cache the response.
	_ = os.WriteFile(manifestPath, body, 0o644)
	if newETag := resp.Header.Get("ETag"); newETag != "" {
		_ = os.WriteFile(etagPath, []byte(newETag), 0o644)
	}

	return parseManifestJSON(body)
}

// parseManifestJSON decodes a gems.json byte slice and returns the entry list
// plus its SHA-256 hex digest.
func parseManifestJSON(data []byte) ([]CollectionEntry, string, error) {
	sum := fmt.Sprintf("%x", sha256.Sum256(data))
	var raw []collectionEntryJSON
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, "", fmt.Errorf("rbs collection: parse manifest JSON: %w", err)
	}
	entries := make([]CollectionEntry, len(raw))
	for i, r := range raw {
		entries[i] = CollectionEntry{
			Name:    r.Name,
			Version: r.Version,
			Source: CollectionSource{
				Type:     r.Source.Type,
				Repo:     r.Source.Repo,
				Revision: r.Source.Revision,
			},
		}
	}
	return entries, sum, nil
}

// FindEntry finds the best matching CollectionEntry for the given gem name and
// version string. It prefers an exact major.minor match, then falls back to
// the highest available version that is <= gemVersion.
// Returns nil if the gem is not in the collection.
func FindEntry(entries []CollectionEntry, gem, gemVersion string) *CollectionEntry {
	// Normalise gemVersion to major.minor (take first two components).
	majorMinor := toMajorMinor(gemVersion)

	var best *CollectionEntry
	for i := range entries {
		e := &entries[i]
		if e.Name != gem {
			continue
		}
		if e.Version == majorMinor {
			return e // exact match
		}
		if best == nil || versionLess(best.Version, e.Version) {
			best = e
		}
	}
	return best
}

// toMajorMinor reduces a version string like "4.8.3" to "4.8".
func toMajorMinor(v string) string {
	parts := strings.SplitN(v, ".", 3)
	if len(parts) >= 2 {
		return parts[0] + "." + parts[1]
	}
	return v
}

// versionLess returns true when a is strictly less than b using simple numeric
// major.minor comparison.
func versionLess(a, b string) bool {
	aParts := strings.SplitN(a, ".", 2)
	bParts := strings.SplitN(b, ".", 2)
	if len(aParts) == 0 || len(bParts) == 0 {
		return a < b
	}
	if aParts[0] != bParts[0] {
		return numericLess(aParts[0], bParts[0])
	}
	if len(aParts) < 2 || len(bParts) < 2 {
		return len(aParts) < len(bParts)
	}
	return numericLess(aParts[1], bParts[1])
}

// numericLess compares two numeric string tokens.
func numericLess(a, b string) bool {
	// Pad with leading zeros to make string comparison work for small integers.
	for len(a) < len(b) {
		a = "0" + a
	}
	for len(b) < len(a) {
		b = "0" + b
	}
	return a < b
}

// FetchRBS fetches the .rbs files for a specific gem from the collection
// tarball. It downloads the full gem_rbs_collection archive and extracts only
// the relevant gem directory. Returns a GemSurface with
// SourceGemRBSCollection, or nil if the gem is not present in the collection.
func (f *Fetcher) FetchRBS(ctx context.Context, gem, gemVersion string, entries []CollectionEntry) (*GemSurface, error) {
	entry := FindEntry(entries, gem, gemVersion)
	if entry == nil {
		return nil, nil
	}

	base := f.cacheBase()
	if err := os.MkdirAll(base, 0o755); err != nil {
		return nil, fmt.Errorf("rbs collection: create cache dir: %w", err)
	}

	// Check for a cached extracted directory.
	gemCacheDir := filepath.Join(base, "gems", gem, entry.Version)
	if info, err := os.Stat(gemCacheDir); err == nil && info.IsDir() {
		return ParseFromDir(gem, gemVersion, gemCacheDir)
	}

	// Download the archive.
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, f.BaseURL, nil)
	if err != nil {
		return nil, fmt.Errorf("rbs collection: build archive request: %w", err)
	}
	resp, err := f.httpClient().Do(req)
	if err != nil {
		return nil, fmt.Errorf("rbs collection: fetch archive: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("rbs collection: archive HTTP %d", resp.StatusCode)
	}

	// Stream-extract only the gem's RBS files.
	var rbsContents []string
	var sha256Sum string
	rbsContents, sha256Sum, err = extractGemRBS(resp.Body, gem, entry.Version)
	if err != nil {
		return nil, fmt.Errorf("rbs collection: extract %s: %w", gem, err)
	}
	if len(rbsContents) == 0 {
		return nil, nil
	}

	classes, err := parseRBSTexts(rbsContents)
	if err != nil {
		return nil, err
	}
	return &GemSurface{
		Gem:       gem,
		Version:   gemVersion,
		RBSSHA256: sha256Sum,
		Source:    SourceGemRBSCollection,
		Classes:   classes,
	}, nil
}

// extractGemRBS reads a tar.gz archive stream and returns the contents of all
// .rbs files under gems/<gem>/<version>/ together with the SHA-256 of the
// concatenated corpus.
func extractGemRBS(r io.Reader, gem, version string) ([]string, string, error) {
	gz, err := gzip.NewReader(r)
	if err != nil {
		return nil, "", err
	}
	defer gz.Close()

	prefix := "gems/" + gem + "/" + version + "/"
	h := sha256.New()
	var contents []string

	tr := tar.NewReader(gz)
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, "", err
		}
		if hdr.Typeflag != tar.TypeReg {
			continue
		}
		clean := filepath.ToSlash(hdr.Name)
		// The archive root is typically "gem_rbs_collection-main/", strip it.
		if idx := strings.Index(clean, "/"); idx >= 0 {
			clean = clean[idx+1:]
		}
		if !strings.HasPrefix(clean, prefix) {
			continue
		}
		if filepath.Ext(clean) != ".rbs" {
			continue
		}
		data, err := io.ReadAll(tr)
		if err != nil {
			return nil, "", err
		}
		h.Write(data)
		contents = append(contents, string(data))
	}
	return contents, fmt.Sprintf("%x", h.Sum(nil)), nil
}
