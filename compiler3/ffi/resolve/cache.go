package resolve

import (
	"bytes"
	"crypto/sha256"
	"encoding/gob"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
)

// cacheVersion is the on-disk format version. Bump in lockstep with
// any change to PackageBinding's gob representation. Loads of a file
// carrying a different version are refused.
const cacheVersion byte = 0x01

// Cache is a per-import-path on-disk binding cache. Each entry is a
// gob-encoded PackageBinding under <Dir>/<safe-import-path>@<gosum>.bin
// with a leading version byte.
//
// Concurrency: Cache holds an in-process mutex over its memo table; on
// disk, writes are atomic via temp-file rename, so concurrent Mochi
// builds in separate processes never corrupt an entry.
type Cache struct {
	// Dir is the on-disk root. Default: $XDG_CACHE_HOME/mochi/bindings
	// (or platform fallback). Created on first Store.
	Dir string

	mu   sync.Mutex
	memo map[string]*PackageBinding
}

// NewCache returns a Cache rooted at the user's platform-appropriate
// cache directory. The directory is created lazily on first Store.
func NewCache() *Cache {
	return &Cache{Dir: DefaultCacheDir()}
}

// DefaultCacheDir reports the cache root: $XDG_CACHE_HOME/mochi/bindings
// on Unix, %LOCALAPPDATA%/mochi/bindings on Windows, and ~/.cache/mochi/
// bindings as the fallback when neither is set.
func DefaultCacheDir() string {
	if runtime.GOOS == "windows" {
		if local := os.Getenv("LOCALAPPDATA"); local != "" {
			return filepath.Join(local, "mochi", "bindings")
		}
	}
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "bindings")
	}
	home, err := os.UserHomeDir()
	if err == nil && home != "" {
		return filepath.Join(home, ".cache", "mochi", "bindings")
	}
	return filepath.Join(os.TempDir(), "mochi", "bindings")
}

// Load returns the cached binding if present and the entry's
// GoSumHash matches the supplied one. The first hit per process is
// served from disk; subsequent hits are served from the in-memory
// memo (sub-microsecond).
func (c *Cache) Load(importPath, goSumHash string) (*PackageBinding, bool) {
	if c == nil {
		return nil, false
	}
	key := cacheKey(importPath, goSumHash)
	c.mu.Lock()
	if c.memo != nil {
		if pb, ok := c.memo[key]; ok {
			c.mu.Unlock()
			return pb, true
		}
	}
	c.mu.Unlock()

	path := c.path(importPath, goSumHash)
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, false
	}
	if len(data) == 0 || data[0] != cacheVersion {
		return nil, false
	}
	pb, err := decodeBinding(data[1:])
	if err != nil {
		return nil, false
	}
	if pb.MochiVersion != MochiResolverVersion {
		return nil, false
	}
	if pb.GoSumHash != goSumHash {
		return nil, false
	}

	c.mu.Lock()
	if c.memo == nil {
		c.memo = map[string]*PackageBinding{}
	}
	c.memo[key] = pb
	c.mu.Unlock()
	return pb, true
}

// Store persists a binding. Returns an error only if the directory
// cannot be created or the file cannot be written; callers that
// cannot tolerate cache failures may ignore the error.
func (c *Cache) Store(pb *PackageBinding) error {
	if c == nil || pb == nil {
		return nil
	}
	if err := os.MkdirAll(c.Dir, 0o755); err != nil {
		return err
	}
	body, err := encodeBinding(pb)
	if err != nil {
		return err
	}
	out := make([]byte, 0, 1+len(body))
	out = append(out, cacheVersion)
	out = append(out, body...)

	path := c.path(pb.ImportPath, pb.GoSumHash)
	tmp, err := os.CreateTemp(c.Dir, ".bind-*")
	if err != nil {
		return err
	}
	if _, err := tmp.Write(out); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return err
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmp.Name())
		return err
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		os.Remove(tmp.Name())
		return err
	}

	c.mu.Lock()
	if c.memo == nil {
		c.memo = map[string]*PackageBinding{}
	}
	c.memo[cacheKey(pb.ImportPath, pb.GoSumHash)] = pb
	c.mu.Unlock()
	return nil
}

// Invalidate drops the cache entry for one import path (any go.sum
// hash). Used by `mochi clean` and by tests.
func (c *Cache) Invalidate(importPath string) error {
	if c == nil {
		return nil
	}
	c.mu.Lock()
	for k := range c.memo {
		if strings.HasPrefix(k, importPath+"@") {
			delete(c.memo, k)
		}
	}
	c.mu.Unlock()

	entries, err := os.ReadDir(c.Dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}
	prefix := safeFilename(importPath) + "@"
	for _, e := range entries {
		if !e.IsDir() && strings.HasPrefix(e.Name(), prefix) {
			_ = os.Remove(filepath.Join(c.Dir, e.Name()))
		}
	}
	return nil
}

func (c *Cache) path(importPath, goSumHash string) string {
	return filepath.Join(c.Dir, safeFilename(importPath)+"@"+gosumTag(goSumHash)+".bin")
}

func cacheKey(importPath, goSumHash string) string {
	return importPath + "@" + gosumTag(goSumHash)
}

func gosumTag(goSumHash string) string {
	if goSumHash == "" {
		return "none"
	}
	if len(goSumHash) > 16 {
		return goSumHash[:16]
	}
	return goSumHash
}

// safeFilename turns "encoding/json" into "encoding_json" so it can
// be a single filename component on every OS.
func safeFilename(importPath string) string {
	r := strings.NewReplacer("/", "_", string(filepath.Separator), "_", ":", "_")
	return r.Replace(importPath)
}

// HashGoSum reads a go.sum file and returns its SHA-256 hex digest.
// An empty path or a missing file returns ("", nil), which the
// resolver treats as "no module context".
func HashGoSum(path string) (string, error) {
	if path == "" {
		return "", nil
	}
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return "", nil
		}
		return "", fmt.Errorf("hash go.sum: %w", err)
	}
	sum := sha256.Sum256(data)
	return hex.EncodeToString(sum[:]), nil
}

func encodeBinding(pb *PackageBinding) ([]byte, error) {
	var buf bytes.Buffer
	if err := gob.NewEncoder(&buf).Encode(pb); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func decodeBinding(data []byte) (*PackageBinding, error) {
	pb := &PackageBinding{}
	if err := gob.NewDecoder(bytes.NewReader(data)).Decode(pb); err != nil {
		return nil, err
	}
	return pb, nil
}
