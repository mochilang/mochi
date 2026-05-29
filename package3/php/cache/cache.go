// Package cache implements the content-addressed Composer dist fetcher and
// SHA-256 cache for the MEP-75 PHP bridge.
//
// Dist archives (zip files from Packagist) are stored under:
//
//	<root>/<sha256-hex[0:2]>/<sha256-hex[2:]>.zip
//
// The sha256 is computed after download and verified against the expected
// value from the Packagist dist info. This layout matches the MEP-75 spec §8.
//
// See [website/docs/research/0075/04-packagist-ingest.md] for the design.
package cache

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// Cache is a filesystem-backed content-addressed store for Composer dist
// archives. Safe for concurrent use; Store operations are atomic via
// tmp-then-rename on the same filesystem.
type Cache struct {
	// Root is the cache root directory, e.g. ~/.cache/mochi/php-deps.
	Root string
	// HTTP is the underlying transport for dist downloads.
	HTTP *http.Client
	// UserAgent is sent with outbound download requests.
	UserAgent string
}

const defaultUserAgent = "mochi-php-bridge/0.1 (+https://mochi-lang.dev)"

// NewCache returns a Cache rooted at root.
func NewCache(root string) *Cache {
	return &Cache{
		Root:      root,
		HTTP:      &http.Client{Timeout: 60 * time.Second},
		UserAgent: defaultUserAgent,
	}
}

// DistPath returns the absolute filesystem path for the given SHA-256 hex
// digest. Does not check whether the file exists.
func (c *Cache) DistPath(sha256hex string) (string, error) {
	if err := validateSHA256Hex(sha256hex); err != nil {
		return "", err
	}
	return filepath.Join(c.Root, sha256hex[:2], sha256hex[2:]+".zip"), nil
}

// Has reports whether a dist archive with the given SHA-256 digest is cached.
func (c *Cache) Has(sha256hex string) bool {
	path, err := c.DistPath(sha256hex)
	if err != nil {
		return false
	}
	_, err = os.Stat(path)
	return err == nil
}

// Store downloads the dist archive from distURL, verifies SHA-256 against
// expectedSHA256 (lowercase hex, may be empty to skip), and stores it at the
// content-addressed path. Returns the cache path and the verified digest.
//
// If the digest is already cached (fast path), the download is skipped.
// If verification fails or the download errors, no partial file is left on disk.
func (c *Cache) Store(ctx context.Context, distURL, expectedSHA256 string) (string, string, error) {
	if expectedSHA256 != "" && c.Has(expectedSHA256) {
		path, err := c.DistPath(expectedSHA256)
		return path, expectedSHA256, err
	}

	stagingDir := filepath.Join(c.Root, ".tmp")
	if err := os.MkdirAll(stagingDir, 0o755); err != nil {
		return "", "", fmt.Errorf("cache: mkdir staging: %w", err)
	}
	tmp, err := os.CreateTemp(stagingDir, "php-dist-*.zip.tmp")
	if err != nil {
		return "", "", fmt.Errorf("cache: create tmp: %w", err)
	}
	tmpPath := tmp.Name()
	cleanup := func() {
		tmp.Close()
		os.Remove(tmpPath) //nolint:errcheck
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, distURL, nil)
	if err != nil {
		cleanup()
		return "", "", fmt.Errorf("cache: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	httpClient := c.HTTP
	if httpClient == nil {
		httpClient = http.DefaultClient
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		cleanup()
		return "", "", fmt.Errorf("cache: GET %s: %w", distURL, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		cleanup()
		return "", "", fmt.Errorf("%w: %s", ErrDistNotFound, distURL)
	}
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 512))
		cleanup()
		return "", "", fmt.Errorf("cache: GET %s: status %d: %s", distURL, resp.StatusCode, strings.TrimSpace(string(body)))
	}

	h := sha256.New()
	if _, err := io.Copy(io.MultiWriter(tmp, h), resp.Body); err != nil {
		cleanup()
		return "", "", fmt.Errorf("cache: copy %s: %w", distURL, err)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("cache: close tmp: %w", err)
	}

	gotSHA := hex.EncodeToString(h.Sum(nil))
	if expectedSHA256 != "" && gotSHA != expectedSHA256 {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("%w: %s: have %s want %s", ErrChecksumMismatch, distURL, gotSHA, expectedSHA256)
	}

	dst, err := c.DistPath(gotSHA)
	if err != nil {
		os.Remove(tmpPath)
		return "", "", err
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("cache: mkdir %s: %w", filepath.Dir(dst), err)
	}
	if err := os.Rename(tmpPath, dst); err != nil {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("cache: rename to %s: %w", dst, err)
	}
	return dst, gotSHA, nil
}

// Load opens a cached dist archive by SHA-256 hex digest. The caller must close
// the returned ReadCloser.
func (c *Cache) Load(sha256hex string) (io.ReadCloser, error) {
	path, err := c.DistPath(sha256hex)
	if err != nil {
		return nil, err
	}
	f, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, fmt.Errorf("%w: %s", ErrDistNotCached, sha256hex)
		}
		return nil, fmt.Errorf("cache: open %s: %w", path, err)
	}
	return f, nil
}

// HashReader computes the SHA-256 of the data in r without storing anything.
func HashReader(r io.Reader) (string, error) {
	h := sha256.New()
	if _, err := io.Copy(h, r); err != nil {
		return "", fmt.Errorf("cache: hash: %w", err)
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

func validateSHA256Hex(s string) error {
	if len(s) != 64 {
		return fmt.Errorf("cache: sha256 hex %q: expected 64 chars, got %d", s, len(s))
	}
	if _, err := hex.DecodeString(s); err != nil {
		return fmt.Errorf("cache: sha256 hex %q: invalid hex: %w", s, err)
	}
	return nil
}

// ErrDistNotFound is returned when the dist archive URL returns HTTP 404.
var ErrDistNotFound = errors.New("cache: dist archive not found")

// ErrChecksumMismatch is returned when the downloaded archive's SHA-256 does
// not match the expected value.
var ErrChecksumMismatch = errors.New("cache: dist checksum mismatch")

// ErrDistNotCached is returned by Load when the requested digest is not cached.
var ErrDistNotCached = errors.New("cache: dist not cached")
