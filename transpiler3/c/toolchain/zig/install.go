package zig

import (
	"archive/tar"
	"archive/zip"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"github.com/ulikunitz/xz"
)

// InstallRoot returns the directory under which Zig releases live.
// Order of preference: MOCHI_ZIG_DIR env var, MOCHI_CACHE_DIR (if
// set, namespaces under "zig"), then $HOME/.mochi/cache/zig, then
// os.UserCacheDir()/mochi/zig.
func InstallRoot() (string, error) {
	if v := strings.TrimSpace(os.Getenv("MOCHI_ZIG_DIR")); v != "" {
		return v, nil
	}
	if v := strings.TrimSpace(os.Getenv("MOCHI_CACHE_DIR")); v != "" {
		return filepath.Join(v, "zig"), nil
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".mochi", "cache", "zig"), nil
	}
	if uc, err := os.UserCacheDir(); err == nil && uc != "" {
		return filepath.Join(uc, "mochi", "zig"), nil
	}
	return "", errors.New("transpiler3/c/toolchain/zig: cannot resolve install root")
}

// versionDir is the path that holds the extracted Zig tree for the
// pinned Version on this host. The zig executable inside is at
// versionDir + "/zig" on POSIX, "/zig.exe" on Windows.
func versionDir(root string) string {
	return filepath.Join(root, Version)
}

// Executable returns the absolute path to the `zig` executable
// inside an extracted tree.
func Executable(versionDir string) string {
	name := "zig"
	if runtime.GOOS == "windows" {
		name = "zig.exe"
	}
	return filepath.Join(versionDir, name)
}

var (
	installMu     sync.Mutex
	installCached string
	installErr    error
)

// Install ensures the pinned Zig release is present on disk for the
// current host and returns the absolute path to the `zig`
// executable. Subsequent calls within the same process are O(1).
// The function is safe to call concurrently; the lock guards both
// the cache fast-path and the download/extract slow-path.
//
// On first use: looks up the asset for (GOOS, GOARCH), downloads
// the archive, verifies its SHA-256 against the manifest, extracts
// it under InstallRoot()/<Version>/, and returns the executable
// path. On subsequent uses (same process): returns the memoised
// result. Across processes: skips download+verify if the executable
// already exists at the expected path (we trust on-disk state; the
// SHA-256 is verified only when we actually fetch).
func Install() (string, error) {
	installMu.Lock()
	defer installMu.Unlock()
	if installCached != "" || installErr != nil {
		return installCached, installErr
	}
	root, err := InstallRoot()
	if err != nil {
		installErr = err
		return "", err
	}
	dir := versionDir(root)
	exe := Executable(dir)
	if _, statErr := os.Stat(exe); statErr == nil {
		installCached = exe
		return exe, nil
	}
	key := ManifestKey(runtime.GOOS, runtime.GOARCH)
	if key == "" {
		err := fmt.Errorf("transpiler3/c/toolchain/zig: no Zig %s asset for host %s/%s",
			Version, runtime.GOOS, runtime.GOARCH)
		installErr = err
		return "", err
	}
	asset, ok := Manifest[key]
	if !ok {
		err := fmt.Errorf("transpiler3/c/toolchain/zig: manifest has no entry for %s", key)
		installErr = err
		return "", err
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		installErr = err
		return "", err
	}
	tmpArchive := filepath.Join(dir, "_zig.download")
	if err := downloadVerified(asset.URL, asset.SHA256, tmpArchive); err != nil {
		_ = os.Remove(tmpArchive)
		installErr = fmt.Errorf("transpiler3/c/toolchain/zig: %w", err)
		return "", installErr
	}
	defer os.Remove(tmpArchive)
	if err := extract(tmpArchive, dir, asset.Kind); err != nil {
		installErr = fmt.Errorf("transpiler3/c/toolchain/zig: %w", err)
		return "", installErr
	}
	if _, statErr := os.Stat(exe); statErr != nil {
		installErr = fmt.Errorf("transpiler3/c/toolchain/zig: archive extracted but %s missing: %w", exe, statErr)
		return "", installErr
	}
	installCached = exe
	return exe, nil
}

// downloadVerified fetches url into dst and verifies the body's
// SHA-256 matches wantHex (case-insensitive hex). The downloaded
// file is left in place on success; the caller is responsible for
// deleting it after the archive is extracted.
func downloadVerified(url, wantHex, dst string) error {
	resp, err := http.Get(url)
	if err != nil {
		return fmt.Errorf("download %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download %s: status %s", url, resp.Status)
	}
	f, err := os.Create(dst)
	if err != nil {
		return err
	}
	h := sha256.New()
	if _, err := io.Copy(io.MultiWriter(f, h), resp.Body); err != nil {
		f.Close()
		return err
	}
	if err := f.Close(); err != nil {
		return err
	}
	got := hex.EncodeToString(h.Sum(nil))
	if !strings.EqualFold(got, wantHex) {
		return fmt.Errorf("sha256 mismatch for %s: got %s, want %s", url, got, wantHex)
	}
	return nil
}

// extract handles tar.xz and zip archives. The archive's top-level
// directory (`zig-<arch>-<os>-<version>/`) is stripped so files
// land directly under dst.
func extract(archive, dst, kind string) error {
	switch kind {
	case "tar.xz":
		return extractTarXZ(archive, dst)
	case "zip":
		return extractZip(archive, dst)
	default:
		return fmt.Errorf("unsupported archive kind %q", kind)
	}
}

func extractTarXZ(archive, dst string) error {
	f, err := os.Open(archive)
	if err != nil {
		return err
	}
	defer f.Close()
	xr, err := xz.NewReader(f)
	if err != nil {
		return fmt.Errorf("xz reader: %w", err)
	}
	tr := tar.NewReader(xr)
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}
		rel := stripFirstComponent(hdr.Name)
		if rel == "" {
			continue
		}
		target := filepath.Join(dst, rel)
		if !strings.HasPrefix(target, filepath.Clean(dst)+string(os.PathSeparator)) && target != dst {
			return fmt.Errorf("archive entry %q escapes destination", hdr.Name)
		}
		switch hdr.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, 0o755); err != nil {
				return err
			}
		case tar.TypeReg:
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			out, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, os.FileMode(hdr.Mode)&0o777)
			if err != nil {
				return err
			}
			if _, err := io.Copy(out, tr); err != nil {
				out.Close()
				return err
			}
			if err := out.Close(); err != nil {
				return err
			}
		case tar.TypeSymlink:
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			_ = os.Remove(target)
			if err := os.Symlink(hdr.Linkname, target); err != nil {
				return err
			}
		}
	}
}

func extractZip(archive, dst string) error {
	zr, err := zip.OpenReader(archive)
	if err != nil {
		return err
	}
	defer zr.Close()
	for _, zf := range zr.File {
		rel := stripFirstComponent(zf.Name)
		if rel == "" {
			continue
		}
		target := filepath.Join(dst, rel)
		if !strings.HasPrefix(target, filepath.Clean(dst)+string(os.PathSeparator)) && target != dst {
			return fmt.Errorf("archive entry %q escapes destination", zf.Name)
		}
		if zf.FileInfo().IsDir() {
			if err := os.MkdirAll(target, 0o755); err != nil {
				return err
			}
			continue
		}
		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}
		rc, err := zf.Open()
		if err != nil {
			return err
		}
		out, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, zf.Mode())
		if err != nil {
			rc.Close()
			return err
		}
		if _, err := io.Copy(out, rc); err != nil {
			rc.Close()
			out.Close()
			return err
		}
		rc.Close()
		if err := out.Close(); err != nil {
			return err
		}
	}
	return nil
}

func stripFirstComponent(name string) string {
	name = strings.TrimPrefix(name, "./")
	idx := strings.IndexByte(name, '/')
	if idx < 0 {
		return ""
	}
	return name[idx+1:]
}
