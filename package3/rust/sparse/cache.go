package sparse

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"hash"
	"io"
	"os"
	"path/filepath"

	"lukechampine.com/blake3"
)

// CrateEntry storage layout under a cache root:
//
//	<root>/registry/index/<host>/<bucket>/<name>            -- raw NDJSON
//	<root>/registry/cache/<host>/<name>-<version>.crate     -- tarball
//	<root>/registry/blake3/<first2>/<rest>.crate            -- BLAKE3 alias
//
// The .crate path mirrors cargo's $CARGO_HOME/registry/cache layout so a
// cargo-aware tool (like build-orchestration in Phase 7) can hard-link
// from one to the other when cargo is run as a sub-process.
//
// The BLAKE3 alias is a content-addressed mirror used by Mochi's own
// lockfile (Phase 8). Storing both hashes in the same artefact lets us
// verify against either format depending on which lockfile schema the
// caller holds.

// Cache is a filesystem-backed cache for sparse-index records and
// .crate tarballs. Cache is safe for concurrent use as long as no two
// callers materialise the same crate@version concurrently (the writes
// are not atomic across processes).
type Cache struct {
	// Root is the cache root; e.g. $XDG_CACHE_HOME/mochi/rust.
	Root string

	// Host is the registry host directory (e.g. "index.crates.io").
	// Defaults to "index.crates.io" when empty.
	Host string
}

// NewCache returns a Cache rooted at root. If host is the empty string,
// Cache.Host defaults to "index.crates.io".
func NewCache(root, host string) *Cache {
	if host == "" {
		host = "index.crates.io"
	}
	return &Cache{Root: root, Host: host}
}

// IndexPath returns the absolute filesystem path where the NDJSON index
// file for name should live. The file may or may not exist.
func (c *Cache) IndexPath(name string) (string, error) {
	bucket, err := BucketPath(name)
	if err != nil {
		return "", err
	}
	return filepath.Join(c.Root, "registry", "index", c.Host, filepath.FromSlash(bucket)), nil
}

// CratePath returns the absolute filesystem path of the .crate tarball
// for name@version. The directory layout mirrors cargo's own cache so a
// later cargo-driven build can pick the file up without extra copying.
func (c *Cache) CratePath(name, version string) (string, error) {
	if err := ValidateCrateName(name); err != nil {
		return "", err
	}
	if version == "" {
		return "", fmt.Errorf("sparse: empty version for %q", name)
	}
	return filepath.Join(c.Root, "registry", "cache", c.Host, fmt.Sprintf("%s-%s.crate", name, version)), nil
}

// Blake3Path returns the content-addressed alias path for a .crate
// tarball given its lowercase hex BLAKE3-256 digest.
func (c *Cache) Blake3Path(b3hex string) (string, error) {
	if len(b3hex) < 4 {
		return "", fmt.Errorf("sparse: blake3 digest %q is too short", b3hex)
	}
	if _, err := hex.DecodeString(b3hex); err != nil {
		return "", fmt.Errorf("sparse: invalid blake3 digest %q: %w", b3hex, err)
	}
	return filepath.Join(c.Root, "registry", "blake3", b3hex[:2], b3hex[2:]+".crate"), nil
}

// StoreIndex writes the raw NDJSON bytes for crate name into the index
// directory and returns the path. Existing files are overwritten
// atomically via tmp-then-rename.
func (c *Cache) StoreIndex(name string, body []byte) (string, error) {
	dst, err := c.IndexPath(name)
	if err != nil {
		return "", err
	}
	if err := writeFileAtomic(dst, body, 0o644); err != nil {
		return "", err
	}
	return dst, nil
}

// LoadIndex reads the cached NDJSON for crate name. Returns
// os.ErrNotExist (via errors.Is) if no cached copy exists.
func (c *Cache) LoadIndex(name string) ([]byte, error) {
	dst, err := c.IndexPath(name)
	if err != nil {
		return nil, err
	}
	return os.ReadFile(dst)
}

// StoreCrate streams from r into the cache for name@version, verifying
// the SHA-256 against expectedSha256 (lowercase hex) and also recording
// the BLAKE3-256 digest. The .crate path is returned along with the
// computed BLAKE3-256 hex string.
//
// If expectedSha256 is the empty string, SHA-256 verification is
// skipped (only valid for self-published crates whose index entry lacks
// a cksum, which cargo no longer accepts but legacy mirrors may).
//
// If the SHA-256 does not match, the partial file is removed and
// ErrChecksumMismatch is returned (wrap-friendly).
func (c *Cache) StoreCrate(name, version, expectedSha256 string, r io.Reader) (string, string, error) {
	dst, err := c.CratePath(name, version)
	if err != nil {
		return "", "", err
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return "", "", fmt.Errorf("sparse: mkdir %s: %w", filepath.Dir(dst), err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(dst), ".crate-*.tmp")
	if err != nil {
		return "", "", fmt.Errorf("sparse: tmp: %w", err)
	}
	tmpPath := tmp.Name()
	cleanup := func() {
		tmp.Close()
		os.Remove(tmpPath)
	}
	sha := sha256.New()
	b3 := blake3.New(32, nil)
	mw := io.MultiWriter(tmp, sha, hash.Hash(b3))
	if _, err := io.Copy(mw, r); err != nil {
		cleanup()
		return "", "", fmt.Errorf("sparse: copy %s: %w", dst, err)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("sparse: close tmp: %w", err)
	}
	gotSha := hex.EncodeToString(sha.Sum(nil))
	gotB3 := hex.EncodeToString(b3.Sum(nil))
	if expectedSha256 != "" && gotSha != expectedSha256 {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("%w: %s@%s: have %s want %s", ErrChecksumMismatch, name, version, gotSha, expectedSha256)
	}
	if err := os.Rename(tmpPath, dst); err != nil {
		os.Remove(tmpPath)
		return "", "", fmt.Errorf("sparse: rename %s: %w", dst, err)
	}
	if alias, err := c.Blake3Path(gotB3); err == nil {
		if mkdirErr := os.MkdirAll(filepath.Dir(alias), 0o755); mkdirErr == nil {
			// Link best-effort. Failure here is non-fatal.
			_ = linkOrCopy(dst, alias)
		}
	}
	return dst, gotB3, nil
}

// HasCrate reports whether a .crate tarball is already cached for
// name@version. The file's presence is taken as sufficient evidence
// the previous StoreCrate completed (because rename-after-verify is
// atomic on POSIX file systems).
func (c *Cache) HasCrate(name, version string) bool {
	dst, err := c.CratePath(name, version)
	if err != nil {
		return false
	}
	_, err = os.Stat(dst)
	return err == nil
}

// FetchIndex fetches the index file for name via the given client and
// stores it in the cache, returning the parsed entries. Always
// overwrites any prior cached copy because the sparse protocol does not
// offer a deterministic ETag for index lines.
func (c *Cache) FetchIndex(ctx context.Context, client *Client, name string) ([]CrateEntry, error) {
	target, err := client.IndexURL(name)
	if err != nil {
		return nil, err
	}
	resp, err := getWithUA(ctx, client, target)
	if err != nil {
		return nil, err
	}
	body, err := io.ReadAll(resp.Body)
	resp.Body.Close()
	if err != nil {
		return nil, fmt.Errorf("sparse: read %s: %w", target, err)
	}
	if _, err := c.StoreIndex(name, body); err != nil {
		return nil, err
	}
	return ParseIndex(bytesReader(body))
}

// ErrChecksumMismatch is returned by Cache.StoreCrate when the SHA-256
// of the downloaded body disagrees with the expected hex string from
// the sparse-index entry.
var ErrChecksumMismatch = errors.New("sparse: crate checksum mismatch")

// writeFileAtomic writes data to path via a sibling tmp file plus
// rename. The destination directory is created as needed.
func writeFileAtomic(path string, data []byte, perm os.FileMode) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("sparse: mkdir %s: %w", filepath.Dir(path), err)
	}
	f, err := os.CreateTemp(filepath.Dir(path), filepath.Base(path)+".tmp-*")
	if err != nil {
		return fmt.Errorf("sparse: tmp: %w", err)
	}
	tmp := f.Name()
	if _, err := f.Write(data); err != nil {
		f.Close()
		os.Remove(tmp)
		return fmt.Errorf("sparse: write %s: %w", tmp, err)
	}
	if err := f.Chmod(perm); err != nil {
		f.Close()
		os.Remove(tmp)
		return fmt.Errorf("sparse: chmod %s: %w", tmp, err)
	}
	if err := f.Close(); err != nil {
		os.Remove(tmp)
		return fmt.Errorf("sparse: close %s: %w", tmp, err)
	}
	if err := os.Rename(tmp, path); err != nil {
		os.Remove(tmp)
		return fmt.Errorf("sparse: rename %s -> %s: %w", tmp, path, err)
	}
	return nil
}

// linkOrCopy hard-links src into dst, falling back to a byte copy when
// the link fails (e.g. cross-device). Used for the BLAKE3 alias.
func linkOrCopy(src, dst string) error {
	if err := os.Link(src, dst); err == nil {
		return nil
	}
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
