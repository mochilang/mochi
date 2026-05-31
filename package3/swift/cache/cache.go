// Package cache implements a content-addressed on-disk cache for downloaded
// Swift package archives. Keys are derived from (registry, id, version) tuples
// using SHA-256 to produce a stable, collision-resistant file name.
package cache

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path/filepath"
)

// Cache is an on-disk content-addressed store for Swift package archives.
type Cache struct {
	// Dir is the root directory of the cache.
	Dir string
}

// KeyFor returns the cache key (a hex SHA-256 string) for a given
// registry, package id, and version triple.
func (c *Cache) KeyFor(registry, id, version string) string {
	input := fmt.Sprintf("%s:%s:%s", registry, id, version)
	sum := sha256.Sum256([]byte(input))
	return fmt.Sprintf("%x", sum)
}

// Get returns the path to the cached file for key, and true if it exists.
// Returns ("", false) if the entry is absent.
func (c *Cache) Get(key string) (string, bool) {
	p := filepath.Join(c.Dir, key)
	if _, err := os.Stat(p); err != nil {
		return "", false
	}
	return p, true
}

// Put stores the contents of r under the given key in the cache. Returns the
// path of the stored file.
func (c *Cache) Put(key string, r io.Reader) (string, error) {
	if err := os.MkdirAll(c.Dir, 0o755); err != nil {
		return "", fmt.Errorf("cache: mkdir %s: %w", c.Dir, err)
	}
	p := filepath.Join(c.Dir, key)
	// Write to a temp file then rename for atomicity
	tmp := p + ".tmp"
	f, err := os.Create(tmp)
	if err != nil {
		return "", fmt.Errorf("cache: create temp file: %w", err)
	}
	defer func() {
		f.Close()
		os.Remove(tmp) // best-effort cleanup on error paths
	}()
	if _, err := io.Copy(f, r); err != nil {
		return "", fmt.Errorf("cache: write: %w", err)
	}
	if err := f.Close(); err != nil {
		return "", fmt.Errorf("cache: close temp file: %w", err)
	}
	if err := os.Rename(tmp, p); err != nil {
		return "", fmt.Errorf("cache: rename: %w", err)
	}
	return p, nil
}
