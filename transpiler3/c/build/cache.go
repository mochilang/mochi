package build

import (
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"lukechampine.com/blake3"

	"mochi/transpiler3/c/runtime"
)

// TranspilerVersion is the cache-key namespace for the MEP-45
// pipeline. Bump this when any pipeline change should invalidate
// existing cached binaries even though source bytes did not change
// (for example: a lower or emit pass that produces different C for
// the same input).
const TranspilerVersion = "mep-0045/0"

// CacheRoot returns the cache directory. Order of preference:
// MOCHI_CACHE_DIR env var, then $HOME/.mochi/cache, then
// os.UserCacheDir()/mochi. The dir is created lazily; the returned
// path may not yet exist on disk.
func CacheRoot() (string, error) {
	if v := strings.TrimSpace(os.Getenv("MOCHI_CACHE_DIR")); v != "" {
		return v, nil
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".mochi", "cache"), nil
	}
	if uc, err := os.UserCacheDir(); err == nil && uc != "" {
		return filepath.Join(uc, "mochi"), nil
	}
	return "", errors.New("transpiler3/c/build: cannot resolve cache root (no MOCHI_CACHE_DIR, no $HOME, no os.UserCacheDir)")
}

var (
	runtimeHashOnce sync.Once
	runtimeHashHex  string
	runtimeHashErr  error
)

// runtimeFingerprint returns a stable hex digest of the embedded
// runtime FS. The walk is in lexicographic order so the digest is
// reproducible regardless of host fs iteration order.
func runtimeFingerprint() (string, error) {
	runtimeHashOnce.Do(func() {
		var paths []string
		err := fs.WalkDir(runtime.Files, ".", func(p string, e fs.DirEntry, err error) error {
			if err != nil || e.IsDir() {
				return err
			}
			paths = append(paths, p)
			return nil
		})
		if err != nil {
			runtimeHashErr = err
			return
		}
		sort.Strings(paths)
		h := blake3.New(32, nil)
		for _, p := range paths {
			data, err := runtime.Files.ReadFile(p)
			if err != nil {
				runtimeHashErr = err
				return
			}
			fmt.Fprintf(h, "%s\x00%d\x00", p, len(data))
			h.Write(data)
		}
		runtimeHashHex = hex.EncodeToString(h.Sum(nil))
	})
	return runtimeHashHex, runtimeHashErr
}

// cacheKey returns the BLAKE3 hex digest used as the on-disk cache
// directory name. The hash domain is the tuple
//
//	(transpiler-version, profile, target-triple, runtime-fingerprint,
//	 source-relpath, source-bytes).
//
// Source path is included so two files with identical content (for
// example two empty source files) do not collide; that costs nothing
// and avoids a confusing class of false hits.
func cacheKey(srcPath string, srcBytes []byte, profile, triple, runtimeHash string) string {
	h := blake3.New(32, nil)
	fmt.Fprintf(h, "v=%s\x00p=%s\x00t=%s\x00r=%s\x00s=%s\x00n=%d\x00",
		TranspilerVersion, profile, triple, runtimeHash, filepath.Clean(srcPath), len(srcBytes))
	h.Write(srcBytes)
	return hex.EncodeToString(h.Sum(nil))
}

// cacheBinaryPath returns the on-disk path that holds a cached
// binary for the given key. Layout: <root>/<key[0:2]>/<key>/bin.
func cacheBinaryPath(root, key string) string {
	if len(key) < 2 {
		return filepath.Join(root, key, "bin")
	}
	return filepath.Join(root, key[:2], key, "bin")
}

// cacheLookup checks whether a cached binary exists for key and, if
// so, copies it to dst with mode 0o755. Returns (true, nil) on hit,
// (false, nil) on miss, (false, err) on copy failure.
func cacheLookup(root, key, dst string) (bool, error) {
	src := cacheBinaryPath(root, key)
	st, err := os.Stat(src)
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	if st.IsDir() {
		return false, fmt.Errorf("transpiler3/c/build: cache entry %s is a directory", src)
	}
	if err := copyFile(src, dst, 0o755); err != nil {
		return false, err
	}
	return true, nil
}

// cacheStore writes the produced binary into the cache. Failures are
// returned to the caller; in the driver we treat store failures as
// soft (log only) since the build itself already succeeded.
func cacheStore(root, key, srcBin string) error {
	dst := cacheBinaryPath(root, key)
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	return copyFile(srcBin, dst, 0o755)
}

// copyFile copies src to dst with the given mode. Atomic via
// rename from a temp sibling so concurrent builds never see a
// half-written cache entry.
func copyFile(src, dst string, mode os.FileMode) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(dst), ".tmp-*")
	if err != nil {
		return err
	}
	tmpName := tmp.Name()
	if _, err := io.Copy(tmp, in); err != nil {
		tmp.Close()
		_ = os.Remove(tmpName)
		return err
	}
	if err := tmp.Chmod(mode); err != nil {
		tmp.Close()
		_ = os.Remove(tmpName)
		return err
	}
	if err := tmp.Close(); err != nil {
		_ = os.Remove(tmpName)
		return err
	}
	return os.Rename(tmpName, dst)
}
