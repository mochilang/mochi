// Package cosmocc provides download and resolution for the cosmocc cross-compiler
// used by MEP-45 Phase 13 (APE / Cosmopolitan one-binary-for-every-OS builds).
//
// cosmocc is the C compiler front-end for the Cosmopolitan libc project.
// It produces Actually Portable Executables (.com binaries) that run natively
// on Linux, macOS, Windows, FreeBSD, NetBSD, and OpenBSD from a single file.
//
// Phase 13.1 vendors cosmocc under the Mochi cache directory (avoiding the
// requirement for users to install it manually). The download is gated behind
// an opt-in call to Ensure() and skipped in CI environments where cosmocc is
// provided via MOCHI_COSMOCC_PATH.
package cosmocc

import (
	"archive/zip"
	"bytes"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// Version is the pinned cosmocc release. Tracks cosmo.zip releases.
// Update this constant to upgrade; the download URL is derived from it.
const Version = "3.9.5"

// downloadURL returns the release archive URL for the current host OS/arch.
func downloadURL() (string, error) {
	// cosmocc ships a single zip for all platforms (it's itself portable).
	_ = runtime.GOOS
	return fmt.Sprintf("https://cosmo.zip/pub/cosmocc/cosmocc-%s.zip", Version), nil
}

// InstallRoot returns the directory under which cosmocc releases are cached.
// Order of preference: MOCHI_COSMOCC_DIR env var, MOCHI_CACHE_DIR (namespaced
// under "cosmocc"), then $HOME/.mochi/cache/cosmocc, then OS cache dir.
func InstallRoot() (string, error) {
	if v := strings.TrimSpace(os.Getenv("MOCHI_COSMOCC_DIR")); v != "" {
		return v, nil
	}
	if v := strings.TrimSpace(os.Getenv("MOCHI_CACHE_DIR")); v != "" {
		return filepath.Join(v, "cosmocc"), nil
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".mochi", "cache", "cosmocc"), nil
	}
	if uc, err := os.UserCacheDir(); err == nil && uc != "" {
		return filepath.Join(uc, "mochi", "cosmocc"), nil
	}
	return "", errors.New("transpiler3/c/toolchain/cosmocc: cannot resolve install root")
}

// versionDir is the extracted cosmocc tree for the pinned Version.
func versionDir(root string) string {
	return filepath.Join(root, Version)
}

// Executable returns the absolute path to the cosmocc binary inside an
// extracted tree. On all platforms the binary is cosmocc/bin/cosmocc.
func Executable(vdir string) string {
	name := "cosmocc"
	if runtime.GOOS == "windows" {
		name = "cosmocc.exe"
	}
	return filepath.Join(vdir, "bin", name)
}

// Find returns the path to a usable cosmocc binary, downloading it if
// necessary. Returns ("", nil) when cosmocc is not available and the caller
// should fall back to a user-supplied binary or skip APE builds.
//
// Resolution order:
//  1. MOCHI_COSMOCC_PATH env var (existing Phase 13.0 behaviour).
//  2. Vendored download under InstallRoot()/Version/.
//  3. "cosmocc" on PATH.
func Find() (string, error) {
	if v := strings.TrimSpace(os.Getenv("MOCHI_COSMOCC_PATH")); v != "" {
		return v, nil
	}

	root, err := InstallRoot()
	if err == nil {
		vdir := versionDir(root)
		bin := Executable(vdir)
		if _, statErr := os.Stat(bin); statErr == nil {
			return bin, nil
		}
	}

	// No vendored binary found; return empty so caller falls back to PATH.
	return "", nil
}

// Ensure downloads and unpacks cosmocc if it is not already present in the
// cache. Returns the path to the cosmocc executable. Returns an error if the
// download or extraction fails.
func Ensure() (string, error) {
	if v := strings.TrimSpace(os.Getenv("MOCHI_COSMOCC_PATH")); v != "" {
		return v, nil
	}

	root, err := InstallRoot()
	if err != nil {
		return "", err
	}

	vdir := versionDir(root)
	bin := Executable(vdir)
	if _, err := os.Stat(bin); err == nil {
		return bin, nil
	}

	url, err := downloadURL()
	if err != nil {
		return "", err
	}

	if err := os.MkdirAll(root, 0o755); err != nil {
		return "", fmt.Errorf("cosmocc: mkdir %s: %w", root, err)
	}

	resp, err := http.Get(url) //nolint:noctx
	if err != nil {
		return "", fmt.Errorf("cosmocc: download %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return "", fmt.Errorf("cosmocc: download %s: HTTP %d", url, resp.StatusCode)
	}

	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("cosmocc: read response: %w", err)
	}

	if err := extractZip(data, vdir); err != nil {
		return "", fmt.Errorf("cosmocc: extract: %w", err)
	}

	// Make binaries executable.
	binDir := filepath.Join(vdir, "bin")
	entries, _ := os.ReadDir(binDir)
	for _, e := range entries {
		_ = os.Chmod(filepath.Join(binDir, e.Name()), 0o755)
	}

	return bin, nil
}

// extractZip unpacks a zip archive (given as raw bytes) into destDir.
func extractZip(data []byte, destDir string) error {
	r, err := zip.NewReader(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		return err
	}
	for _, f := range r.File {
		target := filepath.Join(destDir, filepath.FromSlash(f.Name))
		if f.FileInfo().IsDir() {
			if err := os.MkdirAll(target, 0o755); err != nil {
				return err
			}
			continue
		}
		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}
		rc, err := f.Open()
		if err != nil {
			return err
		}
		out, err := os.Create(target)
		if err != nil {
			rc.Close()
			return err
		}
		_, copyErr := io.Copy(out, rc)
		rc.Close()
		out.Close()
		if copyErr != nil {
			return copyErr
		}
	}
	return nil
}
