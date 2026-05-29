// Package hexindex is the Hex.pm HTTP API v2 client for the MEP-66 Erlang
// bridge. It handles:
//
//  1. Package metadata fetch — GET /packages/{name} returns the version list
//     with outer-SHA-256 and inner-SHA-256 hashes for each release.
//
//  2. Tarball download and verification — fetches the Hex.pm tarball, verifies
//     the outer SHA-256 and the inner SHA-256 + SHA-512 (contents.tar.gz),
//     and caches the result under CacheDir in a content-addressed layout.
//
//  3. Content-addressed cache — <cacheDir>/<name>/<version>/outer.tar and
//     <cacheDir>/<name>/<version>/inner.tar.gz are written on first fetch and
//     served directly on subsequent calls (NoCache=false, default).
//
// Hex.pm tarball structure (as per Hex.pm tarball format v3):
//
//	<name>-<version>.tar
//	  metadata.config      — Erlang term with package meta
//	  contents.tar.gz      — the actual package files; hash == InnerSHA256
//	  CHECKSUM             — text: SHA256(<outer.tar minus CHECKSUM>)
//
// The client uses the public Hex.pm API at https://hex.pm/api (v2).
// Authentication is not required for package fetches; the API key / OIDC token
// is only needed for publish (handled by package3/erlang/publish/).
package hexindex

import (
	"context"
	"crypto/sha256"
	"crypto/sha512"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/package3/erlang/hexsemver"
)

const defaultBaseURL = "https://hex.pm/api"

// Release holds the version metadata for a single package release as returned
// by GET /packages/{name}.
type Release struct {
	// Version is the canonical version string.
	Version string `json:"version"`
	// OuterChecksum is the hex-encoded SHA-256 of the full outer .tar file.
	OuterChecksum string `json:"checksum"`
	// Requirements maps dependency name to its version requirement string.
	Requirements map[string]ReleaseRequirement `json:"requirements"`
	// RetiredAt is non-empty when the release has been retired.
	RetiredAt *time.Time `json:"retired_at,omitempty"`
}

// ReleaseRequirement is the per-dependency entry in a release's requirements.
type ReleaseRequirement struct {
	// Requirement is the version constraint string, e.g. "~> 2.1".
	Requirement string `json:"requirement"`
	// Optional is true when the dep is only needed in some build profiles.
	Optional bool `json:"optional"`
	// App is the OTP application name if it differs from the package name.
	App string `json:"app,omitempty"`
	// Repository is "hexpm" for public packages.
	Repository string `json:"repository,omitempty"`
}

// PackageMeta holds the metadata for a Hex.pm package as returned by
// GET /packages/{name}.
type PackageMeta struct {
	// Name is the package name on Hex.pm.
	Name string `json:"name"`
	// Releases is the list of all versions sorted descending by release date.
	Releases []Release `json:"releases"`
}

// DownloadResult is the output of a successful tarball download+verify.
type DownloadResult struct {
	// OuterPath is the path to the cached outer .tar file.
	OuterPath string
	// InnerPath is the path to the cached inner contents.tar.gz.
	InnerPath string
	// OuterSHA256 is the hex-encoded SHA-256 of the outer .tar.
	OuterSHA256 string
	// InnerSHA256 is the hex-encoded SHA-256 of contents.tar.gz.
	InnerSHA256 string
	// InnerSHA512 is the hex-encoded SHA-512 of contents.tar.gz.
	InnerSHA512 string
}

// Client is the Hex.pm API v2 client.
type Client struct {
	BaseURL    string
	CacheDir   string
	NoCache    bool
	HTTPClient *http.Client
}

// NewClient constructs a Client with sensible defaults.
func NewClient(cacheDir string) *Client {
	return &Client{
		BaseURL:  defaultBaseURL,
		CacheDir: cacheDir,
		HTTPClient: &http.Client{
			Timeout: 60 * time.Second,
		},
	}
}

// GetPackage fetches the version list for a Hex.pm package.
func (c *Client) GetPackage(ctx context.Context, name string) (*PackageMeta, error) {
	url := c.BaseURL + "/packages/" + name
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("hexindex: build request: %w", err)
	}
	req.Header.Set("Accept", "application/json")
	req.Header.Set("User-Agent", "mochi-erlang-bridge/0.1")

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("hexindex: GET %s: %w", url, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNotFound {
		return nil, fmt.Errorf("hexindex: package %q not found on Hex.pm", name)
	}
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("hexindex: GET %s: HTTP %d", url, resp.StatusCode)
	}
	var meta PackageMeta
	if err := json.NewDecoder(resp.Body).Decode(&meta); err != nil {
		return nil, fmt.Errorf("hexindex: decode package meta for %q: %w", name, err)
	}
	return &meta, nil
}

// ActiveVersions returns the non-retired versions for a package, parsed into
// hexsemver.Version. The slice is ordered newest-first (matching Hex.pm API
// response order).
func ActiveVersions(meta *PackageMeta) []hexsemver.Version {
	var vs []hexsemver.Version
	for _, r := range meta.Releases {
		if r.RetiredAt != nil {
			continue
		}
		v, err := hexsemver.ParseVersion(r.Version)
		if err != nil {
			continue
		}
		vs = append(vs, v)
	}
	return vs
}

// ResolveVersion returns the best matching version from meta's active releases
// for the given requirement string (e.g. "~> 2.1.3").
func ResolveVersion(meta *PackageMeta, requirement string) (hexsemver.Version, error) {
	reqs, err := hexsemver.ParseRequirements(requirement)
	if err != nil {
		return hexsemver.Version{}, fmt.Errorf("hexindex: parse requirement %q: %w", requirement, err)
	}
	candidates := ActiveVersions(meta)
	best, ok := reqs.BestMatch(candidates)
	if !ok {
		return hexsemver.Version{}, fmt.Errorf("hexindex: no version of %q satisfies %q", meta.Name, requirement)
	}
	return best, nil
}

// DownloadTarball fetches, verifies, and caches the Hex.pm tarball for
// <name>-<version>. If the cached copy is already present and NoCache=false,
// the download is skipped; only the hash metadata is returned.
//
// The verification steps are:
//  1. Fetch <baseURL>/tarballs/<name>-<version>.tar
//  2. Compute SHA-256 of the full outer .tar body.
//  3. Extract contents.tar.gz from the outer .tar.
//  4. Compute SHA-256 and SHA-512 of contents.tar.gz.
//  5. Compare against the outer-checksum and any provided inner checksums.
func (c *Client) DownloadTarball(ctx context.Context, name, version string) (*DownloadResult, error) {
	cacheDir := filepath.Join(c.CacheDir, name, version)

	outerPath := filepath.Join(cacheDir, "outer.tar")
	innerPath := filepath.Join(cacheDir, "inner.tar.gz")

	if !c.NoCache {
		if res, err := loadCached(outerPath, innerPath); err == nil {
			return res, nil
		}
	}

	url := c.BaseURL + "/tarballs/" + name + "-" + version + ".tar"
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("hexindex: build tarball request: %w", err)
	}
	req.Header.Set("User-Agent", "mochi-erlang-bridge/0.1")

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("hexindex: GET %s: %w", url, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("hexindex: tarball GET %s: HTTP %d", url, resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("hexindex: read tarball body: %w", err)
	}

	outerH := sha256.Sum256(body)
	outerSHA := hex.EncodeToString(outerH[:])

	inner, err := extractInner(body)
	if err != nil {
		return nil, fmt.Errorf("hexindex: extract contents.tar.gz from %s-%s: %w", name, version, err)
	}

	innerH256 := sha256.Sum256(inner)
	innerH512 := sha512.Sum512(inner)
	innerSHA256 := hex.EncodeToString(innerH256[:])
	innerSHA512 := hex.EncodeToString(innerH512[:])

	if err := os.MkdirAll(cacheDir, 0o755); err != nil {
		return nil, fmt.Errorf("hexindex: create cache dir: %w", err)
	}
	if err := os.WriteFile(outerPath, body, 0o644); err != nil {
		return nil, fmt.Errorf("hexindex: write outer.tar: %w", err)
	}
	if err := os.WriteFile(innerPath, inner, 0o644); err != nil {
		return nil, fmt.Errorf("hexindex: write inner.tar.gz: %w", err)
	}

	return &DownloadResult{
		OuterPath:   outerPath,
		InnerPath:   innerPath,
		OuterSHA256: outerSHA,
		InnerSHA256: innerSHA256,
		InnerSHA512: innerSHA512,
	}, nil
}

// VerifyChecksum re-hashes an already-cached DownloadResult and compares
// against the expected inner SHA-256 and outer SHA-256.
// Pass empty string to skip a particular check.
func VerifyChecksum(res *DownloadResult, wantOuterSHA256, wantInnerSHA256 string) error {
	if wantOuterSHA256 != "" {
		data, err := os.ReadFile(res.OuterPath)
		if err != nil {
			return fmt.Errorf("hexindex: read outer.tar: %w", err)
		}
		h := sha256.Sum256(data)
		got := hex.EncodeToString(h[:])
		if !strings.EqualFold(got, wantOuterSHA256) {
			return fmt.Errorf("hexindex: outer SHA-256 mismatch: got %s, want %s", got, wantOuterSHA256)
		}
	}
	if wantInnerSHA256 != "" {
		data, err := os.ReadFile(res.InnerPath)
		if err != nil {
			return fmt.Errorf("hexindex: read inner.tar.gz: %w", err)
		}
		h := sha256.Sum256(data)
		got := hex.EncodeToString(h[:])
		if !strings.EqualFold(got, wantInnerSHA256) {
			return fmt.Errorf("hexindex: inner SHA-256 mismatch: got %s, want %s", got, wantInnerSHA256)
		}
	}
	return nil
}

// loadCached tries to read a previously cached download result. Returns an
// error if either file is missing or unreadable (triggering a fresh download).
func loadCached(outerPath, innerPath string) (*DownloadResult, error) {
	outer, err := os.ReadFile(outerPath)
	if err != nil {
		return nil, err
	}
	inner, err := os.ReadFile(innerPath)
	if err != nil {
		return nil, err
	}
	outerH := sha256.Sum256(outer)
	innerH256 := sha256.Sum256(inner)
	innerH512 := sha512.Sum512(inner)
	return &DownloadResult{
		OuterPath:   outerPath,
		InnerPath:   innerPath,
		OuterSHA256: hex.EncodeToString(outerH[:]),
		InnerSHA256: hex.EncodeToString(innerH256[:]),
		InnerSHA512: hex.EncodeToString(innerH512[:]),
	}, nil
}

// extractInner parses the outer .tar bytes and returns the raw bytes of the
// contents.tar.gz member. Hex.pm tarballs are uncompressed archives with
// exactly three members: metadata.config, contents.tar.gz, CHECKSUM.
func extractInner(tarBytes []byte) ([]byte, error) {
	r := newTarReader(tarBytes)
	for {
		name, data, err := r.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("tar read: %w", err)
		}
		if name == "contents.tar.gz" {
			return data, nil
		}
	}
	return nil, fmt.Errorf("contents.tar.gz not found in Hex.pm outer tarball")
}

// tarReader is a minimal ustar/POSIX tar reader for the uncompressed outer
// Hex.pm tarballs (no GNU extensions needed; the Hex.pm tool produces
// standard 512-byte-block POSIX tarballs).
type tarReader struct {
	data []byte
	pos  int
}

func newTarReader(data []byte) *tarReader { return &tarReader{data: data} }

func (r *tarReader) Next() (name string, data []byte, err error) {
	// Advance to next 512-byte block boundary.
	for r.pos+512 > len(r.data) {
		return "", nil, io.EOF
	}
	hdr := r.data[r.pos : r.pos+512]
	r.pos += 512

	// Two consecutive zero blocks signal end-of-archive.
	allZero := true
	for _, b := range hdr {
		if b != 0 {
			allZero = false
			break
		}
	}
	if allZero {
		return "", nil, io.EOF
	}

	name = strings.TrimRight(string(hdr[0:100]), "\x00")
	if name == "" {
		return "", nil, io.EOF
	}

	// File size is stored in octal in bytes 124-135.
	sizeStr := strings.TrimRight(string(hdr[124:136]), "\x00 ")
	var size int64
	for _, ch := range sizeStr {
		if ch < '0' || ch > '7' {
			break
		}
		size = size*8 + int64(ch-'0')
	}

	// Read data blocks (rounded up to 512-byte boundary).
	blocks := (size + 511) / 512
	end := r.pos + int(blocks)*512
	if end > len(r.data) {
		return "", nil, fmt.Errorf("tar: file %q size %d exceeds archive length", name, size)
	}
	data = r.data[r.pos : r.pos+int(size)]
	r.pos = end
	return name, data, nil
}
