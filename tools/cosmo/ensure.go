package cosmo

import (
	"archive/zip"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// EnsureCosmo installs the Cosmopolitan toolchain if needed. It returns nil if
// the library and compiler binary are already present.
// The files are placed under $COSMO_DIR or tools/cosmo/cosmo.

func EnsureCosmo() error {
	dir := os.Getenv("COSMO_DIR")
	if dir == "" {
		dir = filepath.Join("tools", "cosmo", "cosmo")
	}
	libPath := filepath.Join(dir, "libcosmo.a")
	binPath := filepath.Join(dir, "bin", "cosmocc")
	if _, err := os.Stat(libPath); err == nil {
		if _, err := os.Stat(binPath); err == nil {
			return nil
		}
	}

	fmt.Println("\U0001F680 Installing Cosmopolitan...")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}

	rel, err := latestRelease()
	if err != nil {
		return err
	}
	url := fmt.Sprintf("https://github.com/jart/cosmopolitan/releases/download/%s/cosmocc-%s.zip", rel.TagName, rel.TagName)

	resp, err := http.Get(url)
	if err != nil {
		return fmt.Errorf("download cosmo: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download cosmo: %s", resp.Status)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}
	zr, err := zip.NewReader(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		return err
	}
	arch := runtime.GOARCH
	if arch == "amd64" {
		arch = "x86_64"
	}
	var haveLib, haveBin bool
	for _, f := range zr.File {
		dest := filepath.Join(dir, f.Name)
		if f.FileInfo().IsDir() {
			if err := os.MkdirAll(dest, f.Mode()); err != nil {
				return err
			}
			continue
		}
		if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
			return err
		}
		rc, err := f.Open()
		if err != nil {
			return err
		}
		out, err := os.OpenFile(dest, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, f.Mode())
		if err != nil {
			rc.Close()
			return err
		}
		if _, err := io.Copy(out, rc); err != nil {
			out.Close()
			rc.Close()
			return err
		}
		out.Close()
		rc.Close()
		if strings.HasSuffix(f.Name, "libcosmo.a") {
			haveLib = true
		}
		if f.Name == "bin/cosmocc" {
			haveBin = true
		}
	}
	if !haveLib || !haveBin {
		return fmt.Errorf("required files missing in archive")
	}
	return nil
}

type release struct {
	TagName string `json:"tag_name"`
}

func latestRelease() (*release, error) {
	resp, err := http.Get("https://api.github.com/repos/jart/cosmopolitan/releases/latest")
	if err != nil {
		return nil, fmt.Errorf("fetch release: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("fetch release: %s", resp.Status)
	}
	var r release
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	if r.TagName == "" {
		return nil, fmt.Errorf("invalid release response")
	}
	return &r, nil
}
