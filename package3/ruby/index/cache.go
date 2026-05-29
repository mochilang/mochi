package index

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"sync/atomic"
)

// tmpCounter provides unique suffixes for temporary write files so that
// concurrent stores of the same blob do not collide on the .tmp path.
var tmpCounter atomic.Int64

// Cache is a content-addressed store for downloaded .gem files. Gems are
// stored by their SHA-256 hash under a two-level directory structure:
// <cacheDir>/<sha256[0:2]>/<sha256>.gem
type Cache struct {
	// Dir is the root cache directory.
	Dir string
}

// DefaultCacheDir returns the default gem cache directory. It uses
// $XDG_CACHE_HOME/mochi/ruby-gems if XDG_CACHE_HOME is set, otherwise
// ~/.cache/mochi/ruby-gems.
func DefaultCacheDir() string {
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "ruby-gems")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return filepath.Join(".cache", "mochi", "ruby-gems")
	}
	return filepath.Join(home, ".cache", "mochi", "ruby-gems")
}

// NewCache returns a Cache rooted at dir. The directory is created lazily on
// first Store call.
func NewCache(dir string) *Cache {
	return &Cache{Dir: dir}
}

// gemPath returns the filesystem path for a blob identified by sha256hex.
func (c *Cache) gemPath(sha256hex string) string {
	if len(sha256hex) < 2 {
		return filepath.Join(c.Dir, sha256hex+".gem")
	}
	return filepath.Join(c.Dir, sha256hex[:2], sha256hex+".gem")
}

// Store writes data to the cache and returns the hex-encoded SHA-256 of data.
// If an identical blob is already present the write is skipped (idempotent).
func (c *Cache) Store(gem, version, platform string, data []byte) (string, error) {
	sum := sha256.Sum256(data)
	hexSum := hex.EncodeToString(sum[:])

	path := c.gemPath(hexSum)
	// If already present, skip.
	if _, err := os.Stat(path); err == nil {
		return hexSum, nil
	}

	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return "", fmt.Errorf("cache mkdir %s: %w", filepath.Dir(path), err)
	}
	// Write to a uniquely-named temp file first, then rename for atomicity.
	// The unique suffix prevents concurrent stores of the same blob from
	// clobbering each other's tmp file.
	n := tmpCounter.Add(1)
	tmp := path + ".tmp" + strconv.FormatInt(n, 10)
	if err := os.WriteFile(tmp, data, 0o644); err != nil {
		return "", fmt.Errorf("cache write tmp %s: %w", tmp, err)
	}
	if err := os.Rename(tmp, path); err != nil {
		_ = os.Remove(tmp)
		// Another goroutine may have won the race and already placed the file.
		if _, statErr := os.Stat(path); statErr == nil {
			return hexSum, nil
		}
		return "", fmt.Errorf("cache rename %s -> %s: %w", tmp, path, err)
	}
	return hexSum, nil
}

// Fetch reads and returns the cached blob identified by sha256hex.
// It returns an error if the blob is not present.
func (c *Cache) Fetch(sha256hex string) ([]byte, error) {
	path := c.gemPath(sha256hex)
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, fmt.Errorf("cache miss for %s", sha256hex)
		}
		return nil, fmt.Errorf("cache read %s: %w", path, err)
	}
	// Verify integrity.
	sum := sha256.Sum256(data)
	got := hex.EncodeToString(sum[:])
	if got != sha256hex {
		return nil, fmt.Errorf("cache corruption: stored hash %s does not match key %s", got, sha256hex)
	}
	return data, nil
}

// Has reports whether the blob identified by sha256hex is present in the
// cache. It does not verify the stored content.
func (c *Cache) Has(sha256hex string) bool {
	path := c.gemPath(sha256hex)
	_, err := os.Stat(path)
	return err == nil
}
