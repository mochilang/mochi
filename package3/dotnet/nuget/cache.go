package nuget

import (
	"crypto/sha512"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
)

// Cache is a content-addressed filesystem cache for NuGet .nupkg files.
// Files are keyed by their SHA-512 hex digest. Cache is safe for concurrent
// reads; concurrent writes for the same package should be avoided (writes are
// not atomic across processes).
//
// Directory layout:
//
//	<Dir>/<sha512[:2]>/<sha512>.nupkg
type Cache struct {
	// Dir is the cache root directory, e.g. ~/.cache/mochi/dotnet-deps/.
	Dir string
}

// NewCache returns a Cache rooted at dir.
func NewCache(dir string) *Cache {
	return &Cache{Dir: dir}
}

// Store writes the content of r into the cache for package id@version,
// computing the SHA-512 as it writes. Returns the SHA-512 hex string and
// number of bytes written.
func (c *Cache) Store(id, version string, r io.Reader) (sha512hex string, nBytes int64, err error) {
	if id == "" {
		return "", 0, fmt.Errorf("nuget cache: empty package id")
	}
	if version == "" {
		return "", 0, fmt.Errorf("nuget cache: empty version for %q", id)
	}

	// Write to a temp file while computing SHA-512.
	if err := os.MkdirAll(c.Dir, 0o755); err != nil {
		return "", 0, fmt.Errorf("nuget cache: mkdir %s: %w", c.Dir, err)
	}
	tmp, err := os.CreateTemp(c.Dir, ".nupkg-*.tmp")
	if err != nil {
		return "", 0, fmt.Errorf("nuget cache: create temp: %w", err)
	}
	tmpPath := tmp.Name()
	cleanup := func() {
		tmp.Close()
		os.Remove(tmpPath)
	}

	h := sha512.New()
	mw := io.MultiWriter(tmp, h)
	written, copyErr := io.Copy(mw, r)
	if copyErr != nil {
		cleanup()
		return "", 0, fmt.Errorf("nuget cache: write %s@%s: %w", id, version, copyErr)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmpPath)
		return "", 0, fmt.Errorf("nuget cache: close temp: %w", err)
	}

	digest := hex.EncodeToString(h.Sum(nil))
	dst := c.Path(digest)
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		os.Remove(tmpPath)
		return "", 0, fmt.Errorf("nuget cache: mkdir %s: %w", filepath.Dir(dst), err)
	}

	if err := os.Rename(tmpPath, dst); err != nil {
		// If rename fails (cross-device), fall back to copy.
		if copyErr2 := copyFile(tmpPath, dst); copyErr2 != nil {
			os.Remove(tmpPath)
			return "", 0, fmt.Errorf("nuget cache: install %s: %w", dst, copyErr2)
		}
		os.Remove(tmpPath)
	}
	return digest, written, nil
}

// Path returns the absolute filesystem path for a .nupkg keyed by its
// SHA-512 hex digest. The file may or may not exist.
func (c *Cache) Path(sha512hex string) string {
	return filepath.Join(c.Dir, sha512hex[:2], sha512hex+".nupkg")
}

// Has reports whether a .nupkg with the given SHA-512 hex digest is already
// cached.
func (c *Cache) Has(sha512hex string) bool {
	if len(sha512hex) < 4 {
		return false
	}
	_, err := os.Stat(c.Path(sha512hex))
	return err == nil
}

// Open returns a ReadCloser for the cached .nupkg identified by sha512hex.
// Returns an error wrapping os.ErrNotExist when no entry is cached.
func (c *Cache) Open(sha512hex string) (io.ReadCloser, error) {
	if len(sha512hex) < 4 {
		return nil, fmt.Errorf("nuget cache: sha512 digest %q too short", sha512hex)
	}
	f, err := os.Open(c.Path(sha512hex))
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, fmt.Errorf("nuget cache: %q not cached: %w", sha512hex, os.ErrNotExist)
		}
		return nil, fmt.Errorf("nuget cache: open %q: %w", sha512hex, err)
	}
	return f, nil
}

// copyFile copies src to dst as a fallback for cross-device renames.
func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, in)
	return err
}
