package moduleproxy

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// CacheLayout describes the on-disk layout of the module proxy cache.
//
// The layout mirrors Go's own download cache under GOMODCACHE/cache/download,
// keyed by escaped module path + escaped version + artifact kind:
//
//	<root>/<escaped-modpath>/@v/<escaped-version>.info
//	<root>/<escaped-modpath>/@v/<escaped-version>.mod
//	<root>/<escaped-modpath>/@v/<escaped-version>.zip
//	<root>/<escaped-modpath>/@v/<escaped-version>.ziphash
//
// The ziphash file holds the h1: digest of the zip, which doubles as the
// integrity record consumed by the sumdb phase. The mod file is kept
// alongside but its own h1: digest is *not* stored here (it is derived
// on demand via HashGoMod, so writes can be a single round-trip).
//
// Roots are deterministic across machines: the same module@version always
// lands at the same relative path. The cache is safe for concurrent use
// across processes because every artifact write is atomic (write to
// temp file then rename), and concurrent readers see either the old
// file or the new one but never a partial write.
type CacheLayout struct {
	// Root is the absolute directory under which cache files are written.
	Root string
}

// NewCache returns a CacheLayout rooted under the given directory. If
// root is empty the layout falls back to the user's default cache
// directory (XDG_CACHE_HOME on Linux/BSD, ~/Library/Caches on macOS,
// %LocalAppData% on Windows) under a "mochi-go-bridge/modules"
// subdirectory.
func NewCache(root string) (*CacheLayout, error) {
	if root == "" {
		def, err := defaultCacheRoot()
		if err != nil {
			return nil, err
		}
		root = def
	}
	abs, err := filepath.Abs(root)
	if err != nil {
		return nil, fmt.Errorf("moduleproxy: cache root abs: %w", err)
	}
	if err := os.MkdirAll(abs, 0o755); err != nil {
		return nil, fmt.Errorf("moduleproxy: mkdir cache root: %w", err)
	}
	return &CacheLayout{Root: abs}, nil
}

// InfoPath returns the on-disk path for the .info JSON of module@version.
func (c *CacheLayout) InfoPath(modulePath, version string) (string, error) {
	return c.artifactPath(modulePath, version, "info")
}

// ModPath returns the on-disk path for the .mod of module@version.
func (c *CacheLayout) ModPath(modulePath, version string) (string, error) {
	return c.artifactPath(modulePath, version, "mod")
}

// ZipPath returns the on-disk path for the .zip of module@version.
func (c *CacheLayout) ZipPath(modulePath, version string) (string, error) {
	return c.artifactPath(modulePath, version, "zip")
}

// ZipHashPath returns the on-disk path for the .ziphash sidecar.
func (c *CacheLayout) ZipHashPath(modulePath, version string) (string, error) {
	return c.artifactPath(modulePath, version, "ziphash")
}

func (c *CacheLayout) artifactPath(modulePath, version, ext string) (string, error) {
	escMod, err := EscapePath(modulePath)
	if err != nil {
		return "", err
	}
	escVer, err := EscapeVersion(version)
	if err != nil {
		return "", err
	}
	// Replace OS path separators inside the escaped module path with
	// the platform separator. The escaped form is always "/"-delimited,
	// so on windows we need to translate. filepath.FromSlash handles
	// this.
	return filepath.Join(c.Root, filepath.FromSlash(escMod), "@v", escVer+"."+ext), nil
}

// WriteFile writes data to dst atomically. Parents are created as needed.
// The temporary file is in the same directory as dst so the rename is
// guaranteed to be atomic on POSIX and same-volume on Windows.
func WriteFile(dst string, data []byte, mode os.FileMode) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return fmt.Errorf("moduleproxy: mkdir for %s: %w", dst, err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(dst), "."+filepath.Base(dst)+".tmp.")
	if err != nil {
		return fmt.Errorf("moduleproxy: create temp for %s: %w", dst, err)
	}
	tmpName := tmp.Name()
	// Best-effort cleanup if any step below fails.
	cleanup := func() {
		_ = tmp.Close()
		_ = os.Remove(tmpName)
	}
	if _, err := tmp.Write(data); err != nil {
		cleanup()
		return fmt.Errorf("moduleproxy: write %s: %w", dst, err)
	}
	if err := tmp.Chmod(mode); err != nil && runtime.GOOS != "windows" {
		cleanup()
		return fmt.Errorf("moduleproxy: chmod %s: %w", dst, err)
	}
	if err := tmp.Close(); err != nil {
		_ = os.Remove(tmpName)
		return fmt.Errorf("moduleproxy: close %s: %w", dst, err)
	}
	if err := os.Rename(tmpName, dst); err != nil {
		_ = os.Remove(tmpName)
		return fmt.Errorf("moduleproxy: rename %s: %w", dst, err)
	}
	return nil
}

// StoreZip writes the zip body for module@version into the cache and
// records its h1: digest in the sidecar .ziphash file. The zip body is
// streamed from src; src must produce the entire body for HashZip to
// validate prefix entries.
//
// StoreZip is verify-on-store: if the entries inside the zip do not all
// share the "<modulePath>@<version>/" prefix, the function returns an
// error and the zip is *not* persisted.
func (c *CacheLayout) StoreZip(modulePath, version string, src io.Reader) (string, error) {
	zipPath, err := c.ZipPath(modulePath, version)
	if err != nil {
		return "", err
	}
	buf, err := io.ReadAll(src)
	if err != nil {
		return "", fmt.Errorf("moduleproxy: read zip body: %w", err)
	}
	digest, err := HashZip(bytes.NewReader(buf), modulePath, version)
	if err != nil {
		return "", err
	}
	if err := WriteFile(zipPath, buf, 0o644); err != nil {
		return "", err
	}
	hashPath, err := c.ZipHashPath(modulePath, version)
	if err != nil {
		return "", err
	}
	if err := WriteFile(hashPath, []byte(digest+"\n"), 0o644); err != nil {
		return "", err
	}
	return digest, nil
}

// StoreMod writes the go.mod body for module@version into the cache.
// Returns the h1: digest of the mod file (which is the "<v>/go.mod"
// line later written into go.sum).
func (c *CacheLayout) StoreMod(modulePath, version string, modBytes []byte) (string, error) {
	modPath, err := c.ModPath(modulePath, version)
	if err != nil {
		return "", err
	}
	if err := WriteFile(modPath, modBytes, 0o644); err != nil {
		return "", err
	}
	return HashGoMod(modBytes), nil
}

// StoreInfo writes the raw .info JSON body into the cache.
func (c *CacheLayout) StoreInfo(modulePath, version string, infoBytes []byte) error {
	infoPath, err := c.InfoPath(modulePath, version)
	if err != nil {
		return err
	}
	return WriteFile(infoPath, infoBytes, 0o644)
}

// Has returns true if every artifact (info, mod, zip, ziphash) for
// module@version exists in the cache.
func (c *CacheLayout) Has(modulePath, version string) bool {
	for _, ext := range []string{"info", "mod", "zip", "ziphash"} {
		p, err := c.artifactPath(modulePath, version, ext)
		if err != nil {
			return false
		}
		if _, err := os.Stat(p); err != nil {
			return false
		}
	}
	return true
}

// VerifyZip recomputes the h1: digest of the on-disk zip and compares
// it against the stored .ziphash sidecar. Returns ErrCacheCorrupt if
// the file is missing or the digest no longer matches.
func (c *CacheLayout) VerifyZip(modulePath, version string) error {
	zipPath, err := c.ZipPath(modulePath, version)
	if err != nil {
		return err
	}
	hashPath, err := c.ZipHashPath(modulePath, version)
	if err != nil {
		return err
	}
	zipBytes, err := os.ReadFile(zipPath)
	if err != nil {
		return fmt.Errorf("%w: read zip: %v", ErrCacheCorrupt, err)
	}
	want, err := os.ReadFile(hashPath)
	if err != nil {
		return fmt.Errorf("%w: read ziphash: %v", ErrCacheCorrupt, err)
	}
	wantStr := strings.TrimSpace(string(want))
	got, err := HashZip(bytes.NewReader(zipBytes), modulePath, version)
	if err != nil {
		return fmt.Errorf("%w: rehash: %v", ErrCacheCorrupt, err)
	}
	if got != wantStr {
		return fmt.Errorf("%w: %s@%s digest mismatch: got %s, want %s",
			ErrCacheCorrupt, modulePath, version, got, wantStr)
	}
	return nil
}

// ErrCacheCorrupt is returned when a cached artifact fails its
// integrity check. Callers should typically delete the corresponding
// files and refetch.
var ErrCacheCorrupt = errors.New("moduleproxy: cache corrupt")

// FingerprintBytes returns a stable lowercase hex sha256 of data. It
// is independent of the h1: dirhash; it is suitable for short
// content-fingerprinting where the dirhash framing would be overkill
// (e.g. logging, test assertions).
func FingerprintBytes(data []byte) string {
	sum := sha256.Sum256(data)
	return hex.EncodeToString(sum[:])
}

// defaultCacheRoot picks a per-OS default directory for the bridge's
// module cache. It does *not* reach into the Go toolchain's GOMODCACHE
// because the bridge cache is conceptually separate (different
// integrity policy, different layout guarantees).
func defaultCacheRoot() (string, error) {
	if env := os.Getenv("MOCHI_GO_BRIDGE_CACHE"); env != "" {
		return env, nil
	}
	dir, err := os.UserCacheDir()
	if err != nil {
		return "", fmt.Errorf("moduleproxy: locate user cache dir: %w", err)
	}
	return filepath.Join(dir, "mochi-go-bridge", "modules"), nil
}

