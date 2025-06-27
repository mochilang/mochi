package cosmo

import (
	"archive/zip"
	"bytes"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// EnsureCosmo verifies that the static Cosmopolitan library is installed.
// The library is expected at $COSMO_LIB or tools/cosmo/cosmo/libcosmo.a.

func EnsureCosmo() error {
	path := os.Getenv("COSMO_LIB")
	if path == "" {
		path = filepath.Join("tools", "cosmo", "cosmo", "libcosmo.a")
	}
	if _, err := os.Stat(path); err == nil {
		return nil
	}

	fmt.Println("\U0001F680 Installing Cosmopolitan...")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}

	arch := runtime.GOARCH
	if arch == "amd64" {
		arch = "x86_64"
	}
	url := fmt.Sprintf("https://github.com/jart/cosmopolitan/releases/latest/download/cosmocc-%s.zip", arch)

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
	for _, f := range zr.File {
		if strings.HasSuffix(f.Name, "libcosmo.a") {
			rc, err := f.Open()
			if err != nil {
				return err
			}
			defer rc.Close()
			out, err := os.Create(path)
			if err != nil {
				return err
			}
			defer out.Close()
			if _, err := io.Copy(out, rc); err != nil {
				return err
			}
			return nil
		}
	}
	return fmt.Errorf("libcosmo.a not found in archive")
}
