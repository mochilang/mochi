package maven

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
)

// ErrCacheMiss is returned when a requested entry is not in the cache.
var ErrCacheMiss = errors.New("maven: cache miss")

// Cache is a content-addressed store for JAR files. Entries are keyed by
// their SHA-256 hex digest and stored at:
//
//	<CacheDir>/<sha256[0:2]>/<sha256[2:4]>/<sha256>.jar
//
// The layout matches the Rust bridge's cache (package3/rust/sparse/cache.go).
type Cache struct {
	dir string
}

// NewCache creates a Cache rooted at cacheDir. The directory is not created
// eagerly; it is created on the first Put.
func NewCache(cacheDir string) *Cache {
	return &Cache{dir: cacheDir}
}

// Path returns the filesystem path where the given sha256hex entry would live.
func (c *Cache) Path(sha256hex string) string {
	if len(sha256hex) < 4 {
		return filepath.Join(c.dir, sha256hex+".jar")
	}
	return filepath.Join(c.dir, sha256hex[0:2], sha256hex[2:4], sha256hex+".jar")
}

// Has returns true if the entry exists in the cache.
func (c *Cache) Has(sha256hex string) bool {
	_, err := os.Stat(c.Path(sha256hex))
	return err == nil
}

// Get returns the cached entry for sha256hex, or ErrCacheMiss if not present.
func (c *Cache) Get(sha256hex string) ([]byte, error) {
	data, err := os.ReadFile(c.Path(sha256hex))
	if os.IsNotExist(err) {
		return nil, fmt.Errorf("%w: %s", ErrCacheMiss, sha256hex)
	}
	if err != nil {
		return nil, fmt.Errorf("maven: cache get %s: %w", sha256hex, err)
	}
	return data, nil
}

// Put writes data to the cache under sha256hex. The write is atomic: data is
// first written to a temp file in the same directory, then renamed to the
// final path. Concurrent Puts of the same key are safe.
func (c *Cache) Put(sha256hex string, data []byte) error {
	dest := c.Path(sha256hex)
	if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
		return fmt.Errorf("maven: cache mkdir %s: %w", filepath.Dir(dest), err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(dest), "*.jar.tmp")
	if err != nil {
		return fmt.Errorf("maven: cache tmpfile: %w", err)
	}
	tmpPath := tmp.Name()
	defer func() {
		tmp.Close()
		os.Remove(tmpPath) // no-op if rename succeeded
	}()
	if _, err := tmp.Write(data); err != nil {
		return fmt.Errorf("maven: cache write: %w", err)
	}
	if err := tmp.Sync(); err != nil {
		return fmt.Errorf("maven: cache sync: %w", err)
	}
	tmp.Close()
	if err := os.Rename(tmpPath, dest); err != nil {
		return fmt.Errorf("maven: cache rename to %s: %w", dest, err)
	}
	return nil
}
